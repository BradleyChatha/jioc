module jaster.ioc.depinject;

/// Describes the lifetime of a service.
enum ServiceLifetime
{
    /++
     + The service is constructed every single time it is requested.
     +
     + These services are the most expensive to use, as they need to be constructed *every single time* they're requested, which also
     + puts strain on the GC.
     + ++/
    Transient,

    /++
     + The service is only constructed a single time for every `ServiceProvider`, regardless of which scope is used to access it.
     +
     + These services are the least expensive to use, as they are only constructed a single time per `ServiceProvider`.
     + ++/
    Singleton,

    /++
     + The service is constructed once per scope.
     +
     + These services are between `Transient` and `Singleton` in terms of performance. It mostly depends on how often scopes are created/destroyed
     + in your program.
     +
     + See_Also:
     +  `ServiceProvider.createScope`
     + ++/
    Scoped
}

// Mixin for things like asSingletonRuntime, or asTransient. aka: boilerplate
private mixin template ServiceLifetimeFunctions(ServiceLifetime Lifetime)
{
    import std.conv : to;
    enum Suffix         = Lifetime.to!string; // I *wish* this could be `const` instead of `enum`, but it produces a weird `cannot modify struct because immutable members` error.
    enum FullLifetime   = "ServiceLifetime."~Suffix;

    @safe nothrow pure
    public static
    {
        ///
        mixin("alias as"~Suffix~"Runtime = asRuntime!("~FullLifetime~");");

        ///
        mixin("alias as"~Suffix~"(alias BaseType, alias ImplType) = asTemplated!("~FullLifetime~", BaseType, ImplType);");

        ///
        mixin("alias as"~Suffix~"(alias ImplType) = asTemplated!("~FullLifetime~", ImplType, ImplType);");
    }
}

/++
 + Describes a service.
 +
 + This struct shouldn't be created directly, you must use one of the static construction functions.
 +
 + For example, if you wanted the service to be a singleton, you could do `asSingleton!(IBaseType, ImplementationType)`.
 + ++/
struct ServiceInfo
{
    alias FactoryFunc               = Object delegate(ref ServiceScope);
    alias FactoryFuncFor(T)         = T delegate(ref ServiceScope);
    enum  isValidBaseType(T)        = (is(T == class) || is(T == interface));
    enum  isValidImplType(BaseT, T) = (is(T == class) && (is(T : BaseT) || is(T == BaseT)));
    enum  isValidImplType(T)        = isValidImplType!(T, T);

    private
    {
        TypeInfo        _baseType;
        TypeInfo        _implType;
        FactoryFunc     _factory;
        ServiceLifetime _lifetime;
        TypeInfo[]      _dependencies;

        @safe @nogc
        this(TypeInfo baseType, TypeInfo implType, FactoryFunc func, ServiceLifetime lifetime, TypeInfo[] dependencies) nothrow pure
        {
            this._baseType      = baseType;
            this._implType      = implType;
            this._factory       = func;
            this._lifetime      = lifetime;
            this._dependencies  = dependencies;

            assert(func !is null, "The factory function is null. The `asXXXRuntime` functions can't auto-generate one sadly, so provide your own.");
        }
    }

    /// This is mostly for unit tests.
    @safe
    bool opEquals(const ServiceInfo rhs) const pure nothrow
    {
        return 
        (
            this._baseType is rhs._baseType
         && this._implType is rhs._implType
         && this._lifetime == rhs._lifetime
        );
    }

    /// So we can use this struct as an AA key more easily
    @trusted // @trusted since we're only converting a pointer to a number, without doing anything else to it. 
    size_t toHash() const pure nothrow
    {
        const baseTypePointer  = cast(size_t)(cast(void*)this._baseType);
        const implTypePointer  = cast(size_t)(cast(void*)this._implType);
        const lifetimeAsNumber = cast(size_t)this._lifetime;

        // NOTE: This is just something completely random I made up. I'll research into a proper technique eventually, this just has to exist *in some form* for now.
        return (baseTypePointer ^ implTypePointer) * lifetimeAsNumber;
    }

    @safe nothrow pure
    public static
    {
        /++
         + An internal function, public due to necessity, however will be used to explain the `asXXXRuntime` functions.
         +
         + e.g. `asSingletonRuntime`, `asTransientRuntime`, and `asScopedRuntime`.
         +
         + Notes:
         +  Unlike the `asTemplated` constructor (and things like `asSingleton`, `asScoped`, etc.), this function isn't able to produce
         +  a list of dependency types, so therefore will need to be provided by you, the user, if you're hoping to make use of `ServiceProvider`'s
         +  dependency loop guard.
         +
         +  You only need to provide a list of types that the `factory` function will try to directly retrieve from a `ServiceScope`, not the *entire* dependency chain.
         + ++/
        ServiceInfo asRuntime(ServiceLifetime Lifetime)(TypeInfo baseType, TypeInfo implType, FactoryFunc factory, TypeInfo[] dependencies = null)
        {
            return ServiceInfo(baseType, implType, factory, Lifetime, dependencies);
        }

        /++
         + An internal function, public due to necessity, however will be used to explain the `asXXX` functions.
         +
         + e.g. `asSingleton`, `asTransient`, and `asScoped`.
         +
         + Notes:
         +  This constructor is able to automatically generate the list of dependencies, which will allow `ServiceProvider` to check for
         +  dependency loops.
         +
         +  If `factory` is `null`, then the factory becomes a call to `Injector.construct!ImplType`, which should be fine for most cases.
         + ++/
        ServiceInfo asTemplated(ServiceLifetime Lifetime, alias BaseType, alias ImplType)(FactoryFuncFor!ImplType factory = null)
        if(isValidBaseType!BaseType && isValidImplType!(BaseType, ImplType))
        {
            import std.meta : Filter;
            import std.traits : Parameters;

            enum isClassOrInterface(T)  = is(T == class) || is(T == interface);
            alias ImplTypeCtor          = Injector.FindCtor!ImplType;
            alias CtorParams            = Parameters!ImplTypeCtor;
            alias CtorParamsFiltered    = Filter!(isClassOrInterface, CtorParams);

            TypeInfo[] deps;
            deps.length = CtorParamsFiltered.length;

            static foreach(i, dep; CtorParamsFiltered)
                deps[i] = typeid(dep);

            if(factory is null)
                factory = (ref services) => Injector.construct!ImplType(services);

            return ServiceInfo(typeid(BaseType), typeid(ImplType), factory, Lifetime, deps);
        }
    }

    mixin ServiceLifetimeFunctions!(ServiceLifetime.Singleton);
    mixin ServiceLifetimeFunctions!(ServiceLifetime.Transient);
    mixin ServiceLifetimeFunctions!(ServiceLifetime.Scoped);
}
///
//@safe nothrow pure
unittest
{
    static interface I {}
    static class C : I {}

    Object dummyFactory(ref ServiceScope){ return null; }

    // Testing: All 3 aliases can be found, 1 alias per lifetime, which also tests that all lifetimes are handled properly.
    assert(
        ServiceInfo.asSingletonRuntime(typeid(I), typeid(C), &dummyFactory)
        ==
        ServiceInfo(typeid(I), typeid(C), &dummyFactory, ServiceLifetime.Singleton, null)
    );

    assert(
        ServiceInfo.asTransient!(I, C)((ref provider) => new C())
        ==
        ServiceInfo(typeid(I), typeid(C), &dummyFactory, ServiceLifetime.Transient, null) // NOTE: Factory func is ignored in opEquals, so `dummyFactory` here is fine.
    );

    assert(
        ServiceInfo.asScoped!C()
        ==
        ServiceInfo(typeid(C), typeid(C), &dummyFactory, ServiceLifetime.Scoped, null)
    );

    // opEquals and opHash don't care about dependencies (technically I think they should, but meh), so we have to test directly.
    static class WithDeps
    {
        this(C c, I i, int a){}
    }
    
    auto deps = ServiceInfo.asScoped!WithDeps()._dependencies;
    assert(deps.length == 2);
    assert(deps[0] is typeid(C));
    assert(deps[1] is typeid(I));
}

/++
 + Provides access to a service scope.
 +
 + Description:
 +  The idea of having 'scopes' for services comes from the fact that this library was inspired by ASP Core's Dependency Injection,
 +  which provides similar functionality.
 +
 +  In ASP Core there is a need for each HTTP request to have its own 'ecosystem' for services (its own 'scope').
 +
 +  For example, you don't want your database context to be shared between different requests at the same time, as each request needs
 +  to make/discard their own chnages to the database. Having a seperate database context between each request (scope) allows this to be
 +  achieved easily.
 +
 +  For a lot of programs, you probably won't need to use scopes at all, and can simply use `ServiceProvider.defaultScope` for all of your
 +  needs. Use what's best for your case.
 +
 +  See https://docs.microsoft.com/en-us/aspnet/core/fundamentals/dependency-injection as the documentation there is mostly appropriate for this library as well.
 +
 + Master & Slave `ServiceScope`:
 +  There are two variants of `ServiceScope` that can be accessed - master scopes, and slave scopes.
 +
 +  The principal is pretty simple: Master scopes are the 'true' accessor to the scope so therefore contain the ability to also destroy the scope.
 +
 +  Slave scopes, however, can be seen more as a 'reference' accessor to the scope, meaning that they cannot destroy the underlying scope in anyway.
 +
 + Destroying a scope:
 +  There are two ways for a service scope to be destroyed.
 +
 +  1. The master `ServiceScope` object has its dtor run. Because the `ServiceScope` is the master accessor, then the scope's lifetime is directly tied to the `ServiceScope`'s lifetime.
 +
 +  2. A call to `ServiceProvider.destroyScope` is made, where a master `ServiceScope` is passed to it.
 +
 +  The `ServiceScope` object is non-copyable, so can only be moved via functions like `std.algorithm.mutation.move`
 +
 +  Currently when a scope it destroyed nothing really happens except that the `ServiceProvider` clears its cache of service instances for that specific scope, and allows
 +  another scope to be created in its place.
 +
 +  In the future I will be adding more functionality onto when a scope is destroyed, as the current behaviour is a bit undesirable for multiple reasons (e.g.
 +  if you destroy a scope, any uses of a `ServiceScopeAccessor` for the destroyed scope can trigger a bug-check assert).
 +
 + See_Also:
 +  `ServiceProvider.createScope`, `ServiceProvider.destroyScope`
 + ++/
struct ServiceScope
{
    private size_t          _index;
    private ServiceProvider _provider;
    private bool            _isMasterReference; // True = Scope is destroyed via this object. False = Scope can't be destroyed via this object.

    @disable
    this(this);

    ~this()
    {
        if(this._provider !is null && this._isMasterReference)
            this._provider.destroyScope(this);
    }

    /++
     + Attempts to retrieve a service of the given `baseType`, otherwise returns `null`.
     + ++/
    Object getServiceOrNull(TypeInfo baseType)
    {
        return this._provider.getServiceOrNull(baseType, this);
    }

    /++
     + Attempts to retrieve a service of the given base type `T`, otherwise returns `null`.
     + ++/
    T getServiceOrNull(alias T)()
    if(ServiceInfo.isValidBaseType!T)
    {
        auto service = this.getServiceOrNull(typeid(T));
        if(service is null)
            return null;

        auto casted = cast(T)service;
        assert(casted !is null, "Invalid cast.");

        return casted;
    }
    ///
    unittest
    {
        static interface IPrime
        {
            int getPrime();
        }

        static class PrimeThree : IPrime
        {
            int getPrime()
            {
                return 3;
            }
        }

        auto services = new ServiceProvider([ServiceInfo.asTransient!(IPrime, PrimeThree)]);
        auto service = services.defaultScope.getServiceOrNull!IPrime();
        assert(service !is null);
        assert(service.getPrime() == 3);
    }
}

// Not an interface since a testing-specific implementation has no worth atm, and it'd mean making `serviceScope` virtual.
/++
 + A built-in service that allows access to a slave `ServiceScope` for whichever scope this service
 + was constructed for.
 +
 + Description:
 +  Because `ServiceScope` cannot be copied, it can be a bit annoying for certain services to gain access to it should they need
 +  manual access of fetching scoped services.
 +
 +  As an alternative, services can be injected with this helper service which allows the creation of slave `ServiceScope`s.
 +
 + See_Also:
 +  The documentation for `ServiceScope`.
 + ++/
final class ServiceScopeAccessor
{
    // Since ServiceScope can't be copied, and we shouldn't exactly move it from its default location, we need to re-store
    // some of its info for later.
    private ServiceProvider _provider; 
    private size_t          _index;

    private this(ref ServiceScope serviceScope)
    {
        this._provider = serviceScope._provider;
        this._index    = serviceScope._index;
    }

    /++
     + Returns: A slave `ServiceScope`.
     + ++/
    @property @safe @nogc
    ServiceScope serviceScope() nothrow pure
    {
        return ServiceScope(this._index, this._provider, false); // false = Not master scope object.
    }
}

// Not an interface since a testing-specific implementation has little worth atm, and it'd mean making functions virtual.
/++
 + Provides most of the functionality for managing and using services.
 +
 + Dependency_Checking:
 +  During construction, the `ServiceProvider` will perform a check to ensure that none of its registered services contain a 
 +  dependency loop, i.e. making sure that no service directly/indirectly depends on itself.
 +
 +  If you're creating your `ServiceInfo` via the `ServiceInfo.asSingleton`, `ServiceInfo.asTransient`, or `ServiceInfo.asScoped` constructors,
 +  then this functionality is entirely automatic.
 +
 +  If however you're using the `asXXXRuntime` constructor varient, then it is down to the user to provide an array of `TypeInfo` to that constructor,
 +  representing the service's dependencies. Failing to provide the correct data will cause this guard check to not detect the loop, and later down the line
 +  this will cause an infinite loop leading into a crash.
 +
 +  In the future, I may add a `version()` block that will make `ServiceProvider.getServiceOrNull` also perform dependency loop checks. The reason this is not
 +  performed by default is due to performance concerns (especially once your services start to grow in number).
 +
 + Lifetime_Checking:
 +  During construction, the `ServiceProvider` will perform a check to ensure that none of its registered services contains a dependency on
 +  another service with an incompatible lifetime.
 +
 +  Likewise with depenency checking (see section above), you will need to ensure that you provide the correct data when using the `asXXXRuntime` constructors
 +  for `ServiceInfo`.
 +
 +  The following is a table of valid lifetime pairings:
 +
 +      * Transient - [Transient, Singleton]
 +
 +      * Scoped - [Transient, Scoped, Singleton]
 +
 +      * Singleton - [Transient, Singleton]
 + ++/
final class ServiceProvider
{
    import std.typecons : Nullable, nullable;

    alias ServiceInstanceDictionary = Object[ServiceInfo];
    enum BITS_PER_MASK = long.sizeof * 8;

    private
    {
        struct ScopeInfo
        {
            ServiceInstanceDictionary instances;
            ServiceScopeAccessor      accessor; // Written in a way that they only have to be constructed once, so GC isn't as mad.
        }

        ServiceScope                _defaultScope;
        ServiceInfo[]               _allServices;
        ScopeInfo[]                 _scopes;
        ServiceInstanceDictionary   _singletons;
        long[]                      _scopeInUseMasks;

        Object getServiceOrNull(TypeInfo baseType, ref scope ServiceScope serviceScope)
        {
            assert(serviceScope._provider is this, "Attempting to use service scope who does not belong to this `ServiceProvider`.");

            auto infoNullable = this.getServiceInfoForBaseType(baseType);
            if(infoNullable.isNull)
                return null;

            auto info = infoNullable.get();
            final switch(info._lifetime) with(ServiceLifetime)
            {
                case Transient:
                    return info._factory(serviceScope);

                case Scoped:
                    auto ptr = (cast()info in this._scopes[serviceScope._index].instances); // 'cus apparently const is too painful for it to handle.
                    if(ptr !is null)
                        return *ptr;

                    auto instance = info._factory(serviceScope);
                    this._scopes[serviceScope._index].instances[cast()info] = instance;
                    return instance;

                case Singleton:
                    // TODO: Functionise this
                    auto ptr = (cast()info in this._singletons); // 'cus apparently const is too painful for it to handle.
                    if(ptr !is null)
                        return *ptr;

                    auto instance = info._factory(serviceScope);
                    this._singletons[cast()info] = instance;
                    return instance;
            }
        }

        @safe
        ref long getScopeMaskByScopeIndex(size_t index) nothrow
        {
            const indexIntoArray = (index / BITS_PER_MASK);

            if(indexIntoArray >= this._scopeInUseMasks.length)
                this._scopeInUseMasks.length = indexIntoArray + 1;

            return this._scopeInUseMasks[indexIntoArray];
        }

        @safe
        bool isScopeInUse(size_t index) nothrow
        {
            const bitInMask = (index % BITS_PER_MASK);
            const mask      = this.getScopeMaskByScopeIndex(index);

            return (mask & (1 << bitInMask)) > 0;
        }

        @safe
        void setScopeInUse(size_t index, bool isInUse) nothrow
        {
            const bitInMask = (index % BITS_PER_MASK);
            const bitToUse  = (1 << bitInMask);

            if(isInUse)
                this.getScopeMaskByScopeIndex(index) |= bitToUse;
            else
                this.getScopeMaskByScopeIndex(index) &= ~bitToUse;
        }
        ///
        unittest
        {
            auto services = new ServiceProvider(null);
            assert(!services.isScopeInUse(65));

            services.setScopeInUse(65, true);
            assert(services.isScopeInUse(65));
            assert(services._scopeInUseMasks.length == 2);
            assert(services._scopeInUseMasks[1] == 0b10); // 65 % 64 = 1. 1 << 1 = 0b10
            
            services.setScopeInUse(65, false);
            assert(!services.isScopeInUse(65));
        }

        @safe
        void assertLifetimesAreCompatible()
        {
            @safe
            bool areCompatible(const ServiceLifetime consumer, const ServiceLifetime dependency)
            {
                final switch(consumer) with(ServiceLifetime)
                {
                    case Transient:
                    case Singleton:
                        return dependency != Scoped;

                    case Scoped:
                        return true;
                }
            }

            foreach(service; this._allServices)
            {
                foreach(dependency; service._dependencies)
                {
                    auto dependencyInfo = this.getServiceInfoForBaseType(dependency);
                    if(dependencyInfo.isNull)
                        continue;

                    if(!areCompatible(service._lifetime, dependencyInfo.get._lifetime))
                    {
                        import std.format : format;
                        assert(
                            false,
                            "%s service %s cannot depend on %s service %s as their lifetimes are incompatible".format(
                                service._lifetime,
                                service._baseType,
                                dependencyInfo.get._lifetime,
                                dependency
                            )
                        );
                    }
                }
            }
        }
        //
        unittest
        {
            import std.exception : assertThrown, assertNotThrown;

            static class Scoped
            {
            }

            static class Transient
            {
                this(Scoped){}
            }

            static class GoodTransient
            {
            }

            static class GoodScoped
            {
                this(GoodTransient){}
            }

            assertThrown!Throwable(new ServiceProvider([
                ServiceInfo.asScoped!Scoped,
                ServiceInfo.asTransient!Transient
            ]));

            assertNotThrown!Throwable(new ServiceProvider([
                ServiceInfo.asScoped!GoodScoped,
                ServiceInfo.asTransient!GoodTransient
            ]));

            // Uncomment when tweaking the error message.
            // new ServiceProvider([
            //     ServiceInfo.asScoped!Scoped,
            //     ServiceInfo.asTransient!Transient
            // ]);
        }

        @safe
        void assertNoDependencyLoops()
        {
            TypeInfo[] typeToTestStack;
            TypeInfo[] typeToTestStackWhenLoopIsFound; // Keep a copy of the stack once a loop is found, so we can print out extra info.
            
            @trusted
            bool dependsOn(TypeInfo typeToTest, TypeInfo dependencyType)
            {
                import std.algorithm : canFind;

                if(typeToTestStack.canFind!"a is b"(typeToTest))
                    return false; // Since we would've returned true otherwise, which would end the recursion.

                typeToTestStack ~= typeToTest;
                scope(exit) typeToTestStack.length -= 1;

                auto serviceInfoNullable = this.getServiceInfoForBaseType(typeToTest);
                if(serviceInfoNullable.isNull)
                    return false; // Since the service doesn't exist anyway.

                auto serviceInfo = serviceInfoNullable.get();
                foreach(dependency; serviceInfo._dependencies)
                {
                    if(dependency is dependencyType || dependsOn(cast()dependency, dependencyType))
                    {
                        if(typeToTestStackWhenLoopIsFound.length == 0)
                            typeToTestStackWhenLoopIsFound = typeToTestStack.dup;

                        return true;
                    }
                }

                return false;
            }

            foreach(service; this._allServices)
            {
                if(dependsOn(service._baseType, service._baseType))
                {
                    import std.algorithm : map, joiner;
                    import std.format : format;

                    assert(
                        false,
                        "Circular dependency detected, %s depends on itself:\n%s -> %s".format(
                            service._baseType,
                            typeToTestStackWhenLoopIsFound.map!(t => t.toString())
                                                          .joiner(" -> "),
                            service._baseType
                        )
                    );
                }
            }
        }
        ///
        unittest
        {
            import std.exception : assertThrown;

            // Technically shouldn't catch asserts, but this is just for testing.
            assertThrown!Throwable(new ServiceProvider([
                ServiceInfo.asSingleton!CA,
                ServiceInfo.asSingleton!CB
            ]));

            // Uncomment when tweaking with the error message.
            // new ServiceProvider([
            //     ServiceInfo.asSingleton!CA,
            //     ServiceInfo.asSingleton!CB
            // ]);
        }
        version(unittest) // Because of forward referencing being required.
        {
            static class CA
            {
                this(CB){}
            }
            static class CB
            {
                this(CA) {}
            }
        }
    }

    /++
     + Constructs a new `ServiceProvider` that makes use of the given `services`.
     +
     + Builtin_Services:
     +  * [Singleton] `ServiceProvider` - `this`
     +  
     +  * [Scoped] `ServiceScopeAccessor` - A service for easily accessing slave `ServiceScope`s.
     +
     + Assertions:
     +  No service inside of `services` is allowed to directly or indirectly depend on itself.
     +  e.g. A depends on B which depends on A. This is not allowed.
     +
     + Params:
     +  services = Information about all of the services that can be provided.
     + ++/
    this(ServiceInfo[] services)
    {
        this._defaultScope = this.createScope();

        // I'm doing it this weird way to try and make the GC less angry.
        const EXTRA_SERVICES = 2;
        this._allServices.length                = services.length + EXTRA_SERVICES;
        this._allServices[0..services.length]   = services[0..$];
        this._allServices[services.length]      = ServiceInfo.asSingleton!ServiceProvider((ref _) => this); // Allow ServiceProvider to be injected.
        this._allServices[services.length + 1]  = ServiceInfo.asScoped!ServiceScopeAccessor((ref serviceScope) // Add service to allowed services to access their scope's... scope.
        {
            auto instance = this._scopes[serviceScope._index].accessor;
            if(instance !is null)
                return instance;

            instance = new ServiceScopeAccessor(serviceScope);
            this._scopes[serviceScope._index].accessor = instance;

            return instance;            
        });

        this.assertNoDependencyLoops();
        this.assertLifetimesAreCompatible();
    }

    public final
    {
        /++
         + Creates a new scope.
         +
         + Performance:
         +  For creating a scope, most of the performance cost is only made during the very first creation of a scope of a specific
         +  index. e.g. If you create scope[index=1], destroy it, then make another scope[index=1], the second scope should be made faster.
         +
         +  The speed difference is likely negligable either way though.
         +
         +  Most of the performance costs of making a scope will come from the creation of scoped services for each new scope, but those
         +  are only performed lazily anyway.
         +
         + Returns:
         +  A master `ServiceScope`.
         + ++/
        @safe
        ServiceScope createScope()
        {
            size_t index = 0;
            foreach(i; 0..ulong.sizeof * 8)
            {
                if(!this.isScopeInUse(i))
                {
                    index = i;
                    this.setScopeInUse(i, true);
                    break;
                }
            }

            if(this._scopes.length <= index)
                this._scopes.length = (index + 1);

            return ServiceScope(index, this, true);
        }
        ///
        unittest
        {
            import std.format : format;

            // Basic test to make sure the "in use" mask works properly.
            auto provider = new ServiceProvider(null);

            ServiceScope[3] scopes;
            foreach(i; 0..scopes.length)
            {
                scopes[i] = provider.createScope();
                assert(scopes[i]._index == i + 1, format("%s", scopes[i]._index));
            }

            provider.destroyScope(scopes[1]); // Index 2
            assert(scopes[1]._index == 0);
            assert(scopes[1]._provider is null);
            assert(provider._scopeInUseMasks[0] == 0b1011);

            scopes[1] = provider.createScope();
            assert(scopes[1]._index == 2);
            assert(provider._scopeInUseMasks[0] == 0b1111);
        }

        /++
         + Destroys a scope.
         +
         + Behaviour:
         +  Currently, destroying a scope simply means that the `ServiceProvider` can reuse a small amount of memory
         +  whenever a new scope is created.
         +
         +  In the future I'd like to add more functionality, such as detecting scoped services that implement specific
         +  interfaces for things such as `IDisposableService`, `IReusableService`, etc.
         +
         + Params:
         +  serviceScope = The master `ServiceScope` representing the scope to destroy.
         + ++/
        void destroyScope(ref scope ServiceScope serviceScope)
        {
            assert(serviceScope._provider is this, "Attempting to destroy service scope who does not belong to this `ServiceProvider`.");
            assert(this.isScopeInUse(serviceScope._index), "Bug?");
            assert(serviceScope._isMasterReference, "Attempting to destroy service scope who is not the master reference for the scope. (Did this ServiceScope come from ServiceScopeAccessor?)");

            // For now, just clear the AA. Later on I'll want to add more behaviour though.
            this.setScopeInUse(serviceScope._index, false);
            this._scopes[serviceScope._index].instances.clear();

            serviceScope._index = 0;
            serviceScope._provider = null;
        }

        /++
         + Returns:
         +  The `ServiceInfo` for the given `baseType`, or `null` if the `baseType` is not known by this `ServiceProvider`.
         + ++/
        @safe
        Nullable!(const(ServiceInfo)) getServiceInfoForBaseType(TypeInfo baseType) const nothrow pure
        {
            foreach(service; this._allServices)
            {
                if(service._baseType is baseType)
                    return nullable(service);
            }

            return typeof(return).init;
        }

        /// ditto
        Nullable!(const(ServiceInfo)) getServiceInfoForBaseType(alias BaseType)() const
        if(ServiceInfo.isValidBaseType!BaseType)
        {
            return this.getServiceInfoForBaseType(typeid(BaseType));
        }
        ///
        unittest
        {
            static class C {}

            auto info = ServiceInfo.asScoped!C();
            const provider = new ServiceProvider([info]);

            assert(provider.getServiceInfoForBaseType!C() == info);
        }

        @property @safe @nogc
        ref ServiceScope defaultScope() nothrow pure
        {
            return this._defaultScope;
        }
    }
}

/++
 + A static class containing functions to easily construct objects with Dependency Injection, or
 + execute functions where their parameters are injected via Dependency Injection.
 + ++/
static final class Injector
{
    import std.traits : ReturnType, isSomeFunction;

    public static final
    {
        /++
         + Executes a function where all of its parameters are retrieved from the given `ServiceScope`.
         +
         + Limitations:
         +  Currently you cannot provide your own values for parameters that shouldn't be injected.
         +
         + Behaviour:
         +  For parameters that are a `class` or `interface`, an attempt to retrieve them as a service from
         +  `services` is made. These parameters will be `null` if no service for them was found.
         +
         +  For parameters of other types, they are left as their `.init` value.
         +
         + Params:
         +  services = The `ServiceScope` allowing access to any services to be injected into the function call.
         +  func     = The function to execute.
         +
         + Returns:
         +  Whatever `func` returns.
         + ++/
        ReturnType!F execute(F)(ref ServiceScope services, F func)
        if(isSomeFunction!F)
        {
            import std.traits : Parameters;

            alias FuncParams = Parameters!F;
            FuncParams params;

            static foreach(i, ParamT; FuncParams)
            {{
                static if(is(ParamT == class) || is(ParamT == interface))
                    params[i] = services.getServiceOrNull!ParamT;
            }}

            static if(is(ReturnType!F == void))
                func(params);
            else
                return func(params);
        }
        ///
        unittest
        {
            static class AandB
            {
                int a = 1;
                int b = 3;
            }

            static int addAandB(AandB ab)
            {
                return ab.a + ab.b;
            }

            auto services = new ServiceProvider([ServiceInfo.asSingleton!AandB]);
            
            assert(Injector.execute(services.defaultScope, &addAandB) == 4);
        }

        /++
         + Constructs a `class` or `struct` via Dependency Injection.
         +
         + Limitations:
         +  See `Injector.execute`.
         +
         +  There are no guard checks implemented to ensure services with incompatible lifetimes aren't being used together. However, `ServiceProvider` does contain
         +  a check for this, please refer to its documentation.
         +
         +  There are no guard checks implemented to block circular references between services. However, `ServiceProvider` does contain
         +  a check for this, please refer to its documentation.
         +
         + Behaviour:
         +  See `Injector.execute` for what values are injected into the ctor's parameters.
         +
         +  If the type has a normal ctor, then the result of `__traits(getMember, T, "__ctor")` is used as the constructor.
         +  Types with multiple ctors are undefined behaviour.
         +
         +  If the type contains a static function called `injectionCtor`, then that function takes priority over any normal ctor
         +  and will be used to construct the object. Types with multiple `injectionCtor`s are undefined behaviour.
         +
         +  If the type does not contain any of the above ctor functions then:
         +   
         +      * If the type is a class, `new T()` is used (if possible, otherwise compiler error).
         +
         +      * If the type is a `struct`, `T.init` is used.
         +
         + Params:
         +  services = The `ServiceScope` allowing access to any services to be injected into the newly constructed object.
         +
         + Returns:
         +  The newly constructed object.
         + ++/
        T construct(alias T)(ref ServiceScope services)
        if(is(T == class) || is(T == struct))
        {
            import std.traits : Parameters;

            alias Ctor = Injector.FindCtor!T;

            static if(Injector.isStaticFuncCtor!Ctor) // Special ctors like `injectionCtor`
            {
                alias CtorParams = Parameters!Ctor;
                return Injector.execute(services, (CtorParams params) => T.injectionCtor(params));
            }
            else static if(Injector.isBuiltinCtor!Ctor) // Normal ctor
            {
                alias CtorParams = Parameters!Ctor;

                static if(is(T == class))
                    return Injector.execute(services, (CtorParams params) => new T(params));
                else
                    return Injector.execute(services, (CtorParams params) => T(params));
            }
            else // NoValidCtor
            {
                static if(is(T == class))
                    return new T();
                else
                    return T.init;
            }
        }

        /// `FindCtor` will evaluate to this no-op function if it can't find a proper ctor.
        static void NoValidCtor(){}

        /++
         + Finds the most appropriate ctor for use with injection.
         +
         + Notes:
         +  Types that have multiple overloads of a ctor produce undefined behaviour.
         +
         +  You can use `Injector.isBuiltinCtor` and `Injector.isStaticFuncCtor` to determine what type of Ctor was chosen.
         +
         + Returns:
         +  Either the type's `__ctor`, the type's `injectionCtor`, or `NoValidCtor` if no appropriate ctor was found.
         + ++/
        template FindCtor(T)
        {
            static if(__traits(hasMember, T, "injectionCtor"))
                alias FindCtor = __traits(getMember, T, "injectionCtor");
            else static if(__traits(hasMember, T, "__ctor"))
                alias FindCtor = __traits(getMember, T, "__ctor");
            else
                alias FindCtor = NoValidCtor;
        }

        enum isBuiltinCtor(alias F)     = __traits(identifier, F) == "__ctor";
        enum isStaticFuncCtor(alias F)  = !isBuiltinCtor!F && __traits(identifier, F) != "NoValidCtor";
    }
}

// Testing transient services
unittest
{
    static class Transient
    {
        int increment = 0;

        int getI()
        {
            return this.increment++;
        }
    }

    auto services = new ServiceProvider([ServiceInfo.asTransient!Transient]);
    
    auto serviceA = services.defaultScope.getServiceOrNull!Transient();
    auto serviceB = services.defaultScope.getServiceOrNull!Transient();

    assert(serviceA.getI() == 0);
    assert(serviceA.getI() == 1);
    assert(serviceB.getI() == 0);
}

// Testing scoped services
unittest
{
    static class Scoped
    {
        int increment = 0;

        int getI()
        {
            return this.increment++;
        }
    }

    auto services = new ServiceProvider([ServiceInfo.asScoped!Scoped]);
    auto scopeB   = services.createScope();

    auto serviceA1 = services.defaultScope.getServiceOrNull!Scoped();
    auto serviceA2 = services.defaultScope.getServiceOrNull!Scoped(); // So I can test that it's using the same one.
    auto serviceB  = scopeB.getServiceOrNull!Scoped();
    
    assert(serviceA1 is serviceA2, "Scoped didn't work ;(");
    assert(serviceA1.getI() == 0);
    assert(serviceA2.getI() == 1);
    assert(serviceB.getI() == 0);

    services.destroyScope(scopeB);

    scopeB   = services.createScope();
    serviceB = scopeB.getServiceOrNull!Scoped();
    assert(serviceB.getI() == 0);
}

// Testing singleton services
unittest
{
    static class Singleton
    {
        int increment = 0;

        int getI()
        {
            return this.increment++;
        }
    }

    auto services = new ServiceProvider([ServiceInfo.asSingleton!Singleton]);
    auto scopeB   = services.createScope();

    auto serviceA = services.defaultScope.getServiceOrNull!Singleton;
    auto serviceB = scopeB.getServiceOrNull!Singleton;

    assert(serviceA !is null && serviceA is serviceB);
    assert(serviceA.getI() == 0);
    assert(serviceB.getI() == 1);
}

// Testing scope dtor behaviour
unittest
{
    auto services = new ServiceProvider(null);
    auto scopeB = services.createScope();
    assert(scopeB._index == 1);

    // Test #1, seeing if setting a new value for a scope variable performs the dtor properly.
    {
        scopeB = services.createScope();
        assert(scopeB._index == 2);
        assert(services._scopeInUseMasks[0] == 0b101);
    }

    services.destroyScope(scopeB);

    // Test #2, seeing if going out of scope uses the dtor properly (it should if the first case works, but may as well add a test anyway :P)
    {
        auto scopedScope = services.createScope();
        assert(scopedScope._index == 1);

        scopeB = services.createScope();
        assert(scopeB._index == 2);
    }

    assert(services._scopeInUseMasks[0] == 0b101);
    scopeB = services.createScope();
    assert(scopeB._index == 1);
    assert(services._scopeInUseMasks[0] == 0b11);
}

// Test ServiceProvider injection
unittest
{
    auto services = new ServiceProvider(null);

    assert(services.defaultScope.getServiceOrNull!ServiceProvider() is services);
}

// Test ServiceScopeAccessor
unittest
{
    auto services = new ServiceProvider(null);
    
    // Also test to make sure the slaveScope doesn't destroy the scope outright.
    {
        const slaveScope = services.defaultScope.getServiceOrNull!ServiceScopeAccessor().serviceScope;
        assert(slaveScope._provider is services);
        assert(slaveScope._index == 0);
        assert(!slaveScope._isMasterReference);
    }
    assert(services._scopeInUseMasks[0] == 0b1);

    // Test to make sure we only construct the accessor a single time per scope index.
    auto masterScope = services.createScope();
    assert(services._scopes[1].accessor is null);
    assert(masterScope.getServiceOrNull!ServiceScopeAccessor() !is null);
    assert(services._scopes[1].accessor !is null);

    auto accessor = masterScope.getServiceOrNull!ServiceScopeAccessor();
    services.destroyScope(masterScope);
    masterScope = services.createScope();
    assert(services._scopes[1].accessor is accessor);
}