module jaster.ioc.container;

private
{
    import std.traits : isType, ReturnType, isSomeFunction;
}

/// See_Also: `ServiceCollection.addTransient` & `ServiceCollection.addSingleton`
alias ServicePostFactoryFunc(T) = void delegate(scope T);

private alias ServiceFactoryFunc = Object delegate();

private enum ServiceType
{
    ERROR_Unknown,
    Singleton,
    Transient
}

private struct ServiceInfo
{
    TypeInfo interfaceType;
    TypeInfo implementationType;
    ServiceType type;
    ServiceFactoryFunc factoryFunc;
    ServicePostFactoryFunc!Object postFactoryFunc;
}

/++
 + A collection of the information required to create services.
 +
 + This class can't be used on it's own, and should only be accessed via `ServiceProvider.configureServices`.
 +
 + See_Also:
 +  `ServiceProvider` for a more comprehensive overview of services.
 + ++/
final class ServiceCollection
{
    private final
    {
        ServiceProvider _provider;
        ServiceInfo[TypeInfo] _services; // Key is the implementation type.

        ServiceFactoryFunc createFactory(InterfaceT, ImplT : InterfaceT)()
        {
            import std.traits : Parameters;

            return ()
            {
                static if(__traits(hasMember, ImplT, "__ctor"))
                {
                    alias CtorParams = Parameters!(__traits(getMember, ImplT, "__ctor"));
                    CtorParams params;
                    
                    static foreach(i, ParamT; CtorParams)
                    {{
                        static if(is(ParamT == interface) || is(ParamT == class))
                            params[i] = this._provider.getServiceOrNull!ParamT;
                    }}

                    return new ImplT(params);
                }
                else
                    return new ImplT();
            };
        }

        @safe
        void addService(
            TypeInfo interType, 
            TypeInfo implType, 
            ServiceType type, 
            ServiceFactoryFunc factoryFunc, 
            ServicePostFactoryFunc!Object postFactoryFunc
        )
        {
            if(interType in this._services)
                assert(false, "The service of type '"~interType.toString()~"' already has an implementation.");

            ServiceInfo info;
            info.factoryFunc = factoryFunc;
            info.implementationType = implType;
            info.interfaceType = interType;
            info.type = type;
            info.postFactoryFunc = postFactoryFunc;

            this._services[interType] = info;
        }

        @safe
        this(ServiceProvider provider)
        {
            this._provider = provider;
        }
    }

    public final
    {
        /++
         + Adds a singleton service.
         +
         + Description:
         +  A singleton service is a service that will only ever have a single instance, no matter how many times
         +  it is requested.
         +
         + Notes:
         +  The overload of this function, which takes a single template parameter, is used when
         +  the `InterfaceT` and `ImplT` are the same type, which is perfectly fine to do.
         +
         + Params:
         +  InterfaceT      = The interface type. This should ideally be an `interface`, which is then implemented by the `ImplT` type.
         +  ImplT           = The implementation type. The type that implements `InterfaceT`.
         +  postFactoryFunc = This function is ran after any instance of this service is created. You can use this function to
         +                    perform any mandatory operations on the service before it is to be used.
         +
         + See_Also:
         +  `ServiceProvider`'s documentation comment.
         + ++/
        void addSingleton(InterfaceT, ImplT : InterfaceT)(ServicePostFactoryFunc!ImplT postFactoryFunc = null)
        {
            ServicePostFactoryFunc!Object postFunc = null;
            if(postFactoryFunc !is null)
                postFunc = (scope obj) { postFactoryFunc(cast(ImplT)obj); };

            this.addService(
                typeid(InterfaceT),
                typeid(ImplT),
                ServiceType.Singleton,
                this.createFactory!(InterfaceT, ImplT),
                postFunc
            );
        }

        /// ditto
        void addSingleton(InterfaceAndImplT)(ServicePostFactoryFunc!InterfaceAndImplT postFactoryFunc = null)
        {
            this.addSingleton!(InterfaceAndImplT, InterfaceAndImplT)(postFactoryFunc);
        }

        /++
         + Adds a transient service.
         +
         + Description:
         +  A transient service is a service that will have a new instance created **every time it is requested**.
         +  #PreciseGCIsAThingNow
         +
         + Notes:
         +  The overload of this function, which takes a single template parameter, is used when
         +  the `InterfaceT` and `ImplT` are the same type, which is perfectly fine to do.
         +
         + Params:
         +  InterfaceT      = The interface type. This should ideally be an `interface`, which is then implemented by the `ImplT` type.
         +  ImplT           = The implementation type. The type that implements `InterfaceT`.
         +  postFactoryFunc = This function is ran after any instance of this service is created. You can use this function to
         +                    perform any mandatory operations on the service before it is to be used.
         +
         + See_Also:
         +  `ServiceProvider`'s documentation comment.
         + ++/
        void addTransient(InterfaceT, ImplT : InterfaceT)(ServicePostFactoryFunc!ImplT postFactoryFunc = null)
        {
            ServicePostFactoryFunc!Object postFunc = null;
            if(postFactoryFunc !is null)
                postFunc = (scope obj) { postFactoryFunc(cast(ImplT)obj); };

            this.addService(
                typeid(InterfaceT),
                typeid(ImplT),
                ServiceType.Transient,
                this.createFactory!(InterfaceT, ImplT),
                postFunc
            );
        }

        /// ditto
        void addTransient(InterfaceAndImplT)(ServicePostFactoryFunc!InterfaceAndImplT postFactoryFunc = null)
        {
            this.addTransient!(InterfaceAndImplT, InterfaceAndImplT)(postFactoryFunc);
        }
    }
}

private struct ServiceActivator
{
    private
    {
        ServiceProvider _provider;

        @safe
        this(ServiceProvider provider)
        {
            this._provider = provider;
        }
    }

    public
    {
        Object activateSingleton(ServiceInfo service)
        {
            assert(service.type == ServiceType.Singleton, "Internal error");

            auto ptr = (service.interfaceType in this._provider._singletons);
            if(ptr !is null)
                return *ptr;

            auto obj = service.factoryFunc();
            if(service.postFactoryFunc !is null)
                service.postFactoryFunc(obj);

            this._provider._singletons[service.interfaceType] = obj;

            return obj;
        }

        Object activateTransient(ServiceInfo service)
        {
            assert(service.type == ServiceType.Transient, "Internal error");
            auto obj = service.factoryFunc();

            if(service.postFactoryFunc !is null)
                service.postFactoryFunc(obj);

            return obj;
        }
    }
}

/++
 + An IoC (Inversion of control) container, inspired by ASP Core.
 +
 + Description:
 +  Most terminology is taken from ASP Core.
 +
 +  This container can be used with either the Service Locator pattern (via most of the functions in this class), 
 +  and/or with the Dependency Injection pattern via the `injectAndExecute` and `injectAndConstruct` extention functions.
 +
 +  Dependency injection supports both Constructor Injection (`injectAndConstruct`), and Parameter Injection (`injectAndExecute`).
 +
 +  Dependency injection is implemented via extention functions, showing that the user can also make their own varients if the ones provided
 +  aren't sufficient.
 +
 + Lifetimes:
 +  A 'Singleton' service, much like the name implies, is a service that will only ever have a single instance no matter
 +  how many times it is requested.
 +
 +  A 'Transient' service is a service that will have a new instance created **every single time** it is requested.
 +
 + Service:
 +  A service is split into two parts. It's 'interface' type, and it's 'implementation' type.
 +
 +  The 'interface' type (usually called `InterfaceT` in functions) is usually any old interface (as in, actual D code) that defines
 +  the functions that make up the service.
 +
 +  The `implementation` type (usually called `ImplT`) is then a class providing the implementation of that interface.
 +
 +  The point is that the code consuming these services shouldn't care about the implementation type, but should only care about the interface type.
 +  Allowing one to swap the implementation type without having to touch the consuming code, as it only sees the interface type.
 +
 + Usage:
 +  First, of course, create an instance of this class.
 +
 +  Second, call `configureServices` to gain access to a `ServiceCollection`.
 +
 +  Third, call `ServiceCollection.addTransient`, `ServiceCollection.addSingleton`, and/or any extention functions like `jaster.ioc.container.configure`
 +
 +  Fourth, you can now use the Service Locator pattern by using the `getRequiredService` and `getServiceOrNull` functions of this class.
 +
 +  Fourth.2, you can now use the Dependency Injection pattern by using the `injectAndConstruct` and `injectAndExecute` extention functions.
 +
 +  Five, ???
 +
 +  Six, profit after you tell the world how awesome D is.
 + ++/
final class ServiceProvider
{
    private final
    {
        ServiceCollection _collection;
        ServiceActivator _activator;
        Object[TypeInfo] _singletons;
        bool _isConfigured;
    }

    public final
    {
        ///
        this()
        {
            this._collection = new ServiceCollection(this);
            this._activator = ServiceActivator(this);
        }

        /++
         + Configures the services for this provider.
         +
         + Description:
         +  This function is similar to the `ConfigureServices` function you generally see in ASP Core apps.
         +
         +  It is where you describe all the services that this provider will be supplying.
         +
         + Assertions:
         +  This function may only be called once.
         +
         + Params:
         +  configurator = The functions that performs the configuration of the service provider.
         +                 Please note that the `ServiceCollection` passed to it is marked as `scope`.
         +
         + See_Also:
         +  `ServiceCollection.addSingleton`
         +
         +  `ServiceCollection.addTransient`
         +
         +  `jaster.ioc.container.configure` & `IConfig`
         + ++/
        void configureServices(void delegate(scope ServiceCollection) configurator)
        {
            assert(!this._isConfigured, "This provider has already been configured.");
            this._isConfigured = true;

            configurator(this._collection);
        }

        /++
         + Gets the specified service, or null if it does not exist.
         +
         + Notes:
         +  `interfaceType` will be the `typeid` of the type passed as the `InterfaceT` template parameter to the
         +  several functions of `ServiceCollection`.
         +
         + Special_Cases:
         +  If `interfaceType` is `typeid(ServiceProvider)`, then the current instance of this class is returned.
         +  This allows services to request the provider itself, for cases when that is needed.
         +
         + Assertions:
         +  Several checks for internal errors.
         +
         + Params:
         +  interfaceType = The interface type of the requested service.
         +
         + Returns:
         +  An instance of the requested service, or `null` if not found.
         + ++/
        Object getServiceOrNull(TypeInfo interfaceType)
        {
            if(interfaceType == typeid(ServiceProvider))
                return this;

            auto infoPtr = (interfaceType in this._collection._services);
            if(infoPtr is null)
                return null;

            Object service = null;
            final switch(infoPtr.type) with(ServiceType)
            {
                case ERROR_Unknown: assert(false, "This should never happen in a normal program");

                case Singleton: service = this._activator.activateSingleton(*infoPtr); break;
                case Transient: service = this._activator.activateTransient(*infoPtr); break;
            }

            assert(service !is null, "This shouldn't happen.");
            return service;
        }

        /++
         + A templated version of the other `getServiceOrNull` overload. Please see this documentation as an extention
         + of the other overload's.
         +
         + Assertions:
         +  The requested service must be castable to `InterfaceT`.
         +
         + Params:
         +  InterfaceT = The interface type of the requested service.
         +               This would be the same type passed to the various functions of `ServiceCollection`.
         +
         + Returns:
         +  The value of the other `getServiceOrNull` overload, where a non-null value is casted to `InterfaceT`.
         + ++/
        InterfaceT getServiceOrNull(InterfaceT)()
        {
            auto service = this.getServiceOrNull(typeid(InterfaceT));
            if(service is null)
                return null;

            auto casted = cast(InterfaceT)service;
            if(casted is null)
            {
                import std.format : format;
                assert(false, format(
                    "Cannot cast service of type '%s' to type '%s'.",
                    service.classinfo.toString(),
                    typeid(InterfaceT).toString()
                ));
            }

            return casted;
        }

        /++
         + Almost the same as `getServiceOrNull`, but throws an exception is the service is not found.
         +
         + Exceptions:
         +  `Exception` if the requested service was not found.
         + ++/
        Object getRequiredService(TypeInfo interfaceType)
        {
            import std.exception : enforce;

            auto service = this.getServiceOrNull(interfaceType);
            enforce(service !is null, "Required service '"~interfaceType.toString()~"' could not be found.");

            return service;
        }

        /// ditto
        InterfaceT getRequiredService(InterfaceT)()
        {
            import std.exception : enforce;

            auto service = this.getServiceOrNull!InterfaceT;
            enforce(service !is null, "Required service '"~typeid(InterfaceT).toString()~"' could not be found.");

            return service;
        }
    }
}
///
unittest
{
    static class Config
    {
        string str;
    }

    static interface ICaclulator
    {
        void add(int a, int b);

        @property
        int value();
    }

    static class Calculator : ICaclulator
    {
        int value_;

        override void add(int a, int b){ value_ = a + b; }

        @property
        override int value() { return value_; }
    }

    static interface ISomeService
    {
        bool trueIfInjectionWorked();
    }

    static class SomeService : ISomeService
    {
        ICaclulator _calc;

        this(ICaclulator calc)
        {
            this._calc = calc;
        }

        override bool trueIfInjectionWorked()
        {
            return this._calc !is null;
        }
    }

    auto provider = new ServiceProvider();
    provider.configureServices((scope services)
    {
        services.addSingleton!Config;
        services.addTransient!(ICaclulator, Calculator);
        services.addTransient!(ISomeService, SomeService);
    });

    // Test singletons actually only use a single instance
    auto single = provider.getRequiredService!Config;
    single.str = "Lalafell";
    single = provider.getRequiredService!Config;
    assert(single.str == "Lalafell");

    // Test transients use a new instance every time
    auto transient = provider.getRequiredService!ICaclulator;
    transient.add(2, 2); // quik maffs
    assert(transient.value == 4);

    transient = provider.getRequiredService!ICaclulator;
    assert(transient.value == int.init);

    // Test constructor injection works
    auto service = provider.getRequiredService!ISomeService;
    assert(service.trueIfInjectionWorked);
}

/++
 + A basic interface for services that act as configuration.
 +
 + Params:
 +  T = The type of data to store.
 + ++/
interface IConfig(alias T)
if(isType!T)
{
    /++
     + Returns:
     +  The value stored in this configuration.
     + ++/
    @property
    T value();
}

private final class SimpleConfig(T) : IConfig!T
{
    T _value;

    @property
    override T value()
    {
        return this._value;
    }
}

/++
 + Configures a piece of data, and adds it to the `ServiceCollection`.
 +
 + Description:
 +  This is a helper extention function to easily add an `IConfig` into a `ServiceCollection`
 +  as a singleton service.
 +
 + Notes:
 +  The configured data can be accessed by requesting the service `IConfig!T`.
 +
 +  The `configurator` function is ran when the configured data is **first requested**.
 +
 + Params:
 +  collection   = The collection to add the configured data to.
 +  configurator = The function that creates the configured data.
 + ++/
void configure(alias T)(ServiceCollection collection, void delegate(scope ref T) configurator)
if(isType!T)
{
    collection.addSingleton!(IConfig!T, SimpleConfig!T)((scope conf) 
    { 
        T value = T.init;
        static if(is(T == class))
            value = new T();

        configurator(value);
        conf._value = value; 
    });
}
///
unittest
{
    static struct Config
    {
        string a;
        int b;
    }

    static interface IIsAEqualToB
    {
        bool result();
    }

    static class IsAEqualToB : IIsAEqualToB
    {
        IConfig!Config _config;

        this(IConfig!Config config)
        {
            this._config = config;
        }

        override bool result()
        {
            import std.conv : to;
            return this._config.value.a.to!int == this._config.value.b;
        }
    }

    auto provider = new ServiceProvider();
    provider.configureServices((scope services)
    {
        services.configure!Config((scope ref c){ c.a = "20"; c.b = 20; });
        services.addSingleton!(IIsAEqualToB, IsAEqualToB);
    });

    auto service = provider.getRequiredService!IIsAEqualToB;
    assert(service.result);
}

/++
 + Executes a function by injecting it's parameters from the given `ServiceProvider`.
 +
 + Description:
 +  This function works by taking each parameter's type, and attempting to retrieve that type from the `ServiceProvider`.
 +
 +  If a parameter was found as a service, then the service is passed as that parameter.
 +
 +  If a parameter was not found as a service, then it's value will be set to `ParamType.init`
 +
 + Limitations:
 +  Currently it is assumed that **all** parameters are to be injected. So it's not possible to inject a few parameters while
 +  also passing in the rest of the parameters' values seperately. However, this is a planned feature.
 +
 + Params:
 +  F           = The function to inject and execute.
 +  provider    = The provider to request the services from.
 +
 + Returns:
 +  Whatever `func` returns.
 + ++/
ReturnType!F injectAndExecute(F)(ServiceProvider provider, F func)
if(isSomeFunction!F)
{
    import std.traits : Parameters;

    alias FuncParams = Parameters!F;
    FuncParams params;

    static foreach(i, ParamT; FuncParams)
    {{
        static if(is(ParamT == class) || is(ParamT == interface))
            params[i] = provider.getServiceOrNull!ParamT;
    }}

    static if(is(ReturnType!F == void))
        func(params);
    else
        return func(params);
}
///
unittest
{
    static class SomeService {}
    static class SomeOtherService {}

    static bool someFunc(SomeService a, SomeOtherService b)
    {
        assert(a !is null);
        assert(b is null);

        return true;
    }

    auto provider = new ServiceProvider();
    provider.configureServices((scope services)
    {
        services.addTransient!SomeService;        
    });

    assert(provider.injectAndExecute(&someFunc));
}

/++
 + A helper function that makes use of `injectAndExecute` to construct an instance of a class.
 +
 + All notes, descriptions, limitations, etc. are described in `injectAndExecute`'s documentation.
 +
 + Limitations:
 +  Currently only classes are supported.
 +
 +  If there are multiple constructors, then whichever one `Parameters!(__traits(getMember, T, "__ctor"))` decides to return
 +  the parameters of will be used. I do plan to add a UDA to specify which one to use.
 +
 + Returns:
 +  A newly constructed `T`, where it's constructor was injected with `injectAndExecute`.
 + ++/
T injectAndConstruct(alias T)(ServiceProvider provider)
if(is(T == class))
{
    import std.traits : Parameters;

    static if(__traits(hasMember, T, "__ctor"))
    {
        alias CtorParams = Parameters!(__traits(getMember, T, "__ctor"));
        return provider.injectAndExecute((CtorParams params) => new T(params));
    }
    else
        return T.init;
}
///
unittest
{
    static struct Config
    {
        string a;
        int b;
    }

    static class NonServiceButStillInjected
    {
        IConfig!Config _config;

        this(IConfig!Config config)
        {
            assert(config !is null);
            this._config = config;
        }

        bool isAEqualToB()
        {
            import std.conv : to;
            return this._config.value.a.to!int == this._config.value.b;
        }
    }

    auto provider = new ServiceProvider();
    provider.configureServices((scope services)
    {
        services.configure!Config((scope ref c){ c.a = "20"; c.b = 20; });
    });

    auto obj = provider.injectAndConstruct!NonServiceButStillInjected();
    assert(obj.isAEqualToB);
}