# Users Module
The `users` module manages the creation, update, removal and authentication
of users in an application.  The module provides the foundations for user
management in a web application.

A user can register himself by using a subscription form.  In that case,
a verification mail is sent and the user has to follow the verification
link defined in the mail to finish the registration process.  The user
will authenticate using a password.

A user can also use an OpenID account and be automatically registered.

A user can have one or several permissions that allow to protect the
application data.  User permissions are managed by the `Permissions.Module`.

## Integration
The `User_Module` manages the creation, update, removal of users
in an application.  It provides operations that are used by the user
beans or other services to create and update wiki pages.
An instance of the `User_Module` must be declared and registered in the
AWA application.  The module instance can be defined as follows:

```Ada
type Application is new AWA.Applications.Application with record
   User_Module : aliased AWA.Users.Modules.User_Module;
end record;
```

And registered in the `Initialize_Modules` procedure by using:

```Ada
Register (App    => App.Self.all'Access,
          Name   => AWA.Users.Modules.NAME,
          Module => App.User_Module'Access);
```


## Configuration
The *users* module uses a set of configuration properties to configure
the OpenID integration.


| Name                      | Description                                                    |
|:--------------------------|:---------------------------------------------------------------|
|openid.realm|The REALM URL used by OpenID providers to verify the validity of the verification callback.|
| |#{app_url_base}/auth|
|openid.callback_url|The verification callback URI used by the OpenID provider to redirect the user after authentication.|
| |#{app_url_base}/auth/verify|
|openid.success_url|The URI where the user is redirected after a successful authentication.|
| |#{contextPath}/workspaces/main.html|
|auth.url.orange|Orange OpenID access point|
| |https://openid.orange.fr|
|auth.provider.orange|Auth provider to use for Orange|
| |openid|
|auth.url.yahoo|Yahoo! OpenID access point|
| |https://open.login.yahooapis.com/openid20/www.yahoo.com/xrds|
|auth.provider.yahoo|Auth provider to use for Yahoo!|
| |openid|
|auth.url.google|Google OpenID access point|
| |https://www.google.com/accounts/o8/id|
|auth.provider.google|Auth provider to use for Google|
| |openid|
|auth.url.facebook|Facebook OAuth access point|
| |https://www.facebook.com/dialog/oauth|
|auth.provider.facebook|Auth provider to use for Facebook|
| |facebook|
|facebook.callback_url|Facebook verify callback|
| |#{app_oauth_url_base}#{contextPath}/auth/verify|
|facebook.request_url|Facebook request OAuth token access point|
| |https://graph.facebook.com/oauth/access_token|
|facebook.scope|Facebook permission scope|
| |public_profile,email|
|facebook.client_id|Facebook API client ID|
| |#{app_facebook_client_id}|
|facebook.secret|Facebook API secret|
| |#{app_facebook_secret}|
|auth.url.google-plus|Google+ OAuth access point|
| |https://accounts.google.com/o/oauth2/auth|
|auth.provider.google-plus|Auth provider to use for Google+|
| |google-plus|
|google-plus.issuer|Google+ issuer identification|
| |accounts.google.com|
|google-plus.callback_url|Google+ verify callback|
| |#{app_oauth_url_base}#{contextPath}/auth/verify|
|google-plus.request_url|Google+ request OAuth token access point|
| |https://accounts.google.com/o/oauth2/token|
|google-plus.scope|Google+ permission scope|
| |openid profile email|
|google-plus.client_id|Google+ API client ID|
| |#{app_google_plus_client_id}|
|google-plus.secret|Google+ API secret|
| |#{app_google_plus_secret}|
|auth-filter.redirect|URI to redirect to the login page|
| |#{contextPath}/auth/login.html|
|verify-filter.redirect|URI to redirect to the login page|
| |#{contextPath}/auth/login.html|



## Ada Beans
Several bean types are provided to represent and manage the users.
The user module registers the bean constructors when it is initialized.
To use them, one must declare a bean definition in the application
XML configuration.


| Name           | Description                                                               |
|:---------------|:--------------------------------------------------------------------------|
|login|This bean is used by the login form|
|register|This bean is used by the registration form|
|resetPassword|This bean is used by the reset password form|
|lostPassword|This bean is used by the lost password form|
|logout|This bean is used by the logout process|
|user|This bean allows to provide information about the current logged user.|





## Data model
![](images/awa_users_model.png)


