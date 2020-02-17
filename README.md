# Very brief, quickly made overview

Modeled after ASP Core's Dependency Injection.

Expect bugs.

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

    auto loginService = services.getServiceOrNull!LoginService();
    loginService.signup("dlang #1");
    loginService.signup("crash me baby");
}
```