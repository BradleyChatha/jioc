# Overview

This library was inspired by ASP Core's [Dependency Injection](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/dependency-injection?view=aspnetcore-3.1).

The `ServiceProvider` class can be used as the Service Locator and/or the Constructor Injection patterns.

The `Injector` class can also inject values into a function call, as well as perform constructor injection on `structs` (whereas `ServiceProvider` is limited to classes).

Expect bugs for the time being, but please don't be afraid to open an issue on Github if you come across a bug, issue, or want to suggest and improvement.

# Usage

More fleshed out README to come soon (TM).

TL;DR

```d
import jaster.ioc;

interface IPasswordMatcher
{
    bool isValidPassword(string pass);
}

class PasswordMatcher : IPasswordMatcher
{
    bool isValidPassword(string pass)
    {
        return pass == "dlang #1";
    }
}

class LoginService
{
    private IPasswordMatcher _matcher;

    this(IPasswordMatcher matcher)
    {
        this._matcher = matcher;
    }

    void signup(string password)
    {
        assert(this._matcher.isValidPassword(password));
    }
}

void main()
{
    auto services = new ServiceProvider(
    [
        ServiceInfo.asSingleton!(IPasswordMatcher, PasswordMatcher),
        ServiceInfo.asScoped!LoginService
    ]);

    auto loginService = services.defaultScope.getServiceOrNull!LoginService();
    loginService.signup("dlang #1");
    loginService.signup("crash me baby");
}
```