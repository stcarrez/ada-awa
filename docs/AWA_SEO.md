# SEO Module
The `AWA.SEO` module provides tools for search engine optimization when publishing
pages that must be discovered and scanned by search engines.  It provides `sitemap`
generation and allows other modules to announce what they publish for search engines
in the form of a sitemap entry.

A sitemap entry is described by the `Sitemap_Entry` type and it is composed of an
absolute URI location, a last modification date, a priority, an optional image URI
location and an optional image title.  A module that wants to provide a list of
sitemap entries must implement the `Sitemap_Provider` limited interface and implement
the `Create_Sitemap` procedure.  That procedure must populate a `Sitemap_Info` record
which contains the sitemap entries that must be exposed.  For example:

```Ada
with AWA.SEO;
...
procedure Create_Sitemap (Provider : in Sitemap_Provider;
                          Sitemap  : in out Sitemap_Info) is
   Item : Sitempap_Entry;
begin
   ...
   Sitemap.Entries.Append (Item);
   ...
end Create_Sitemap;
```

The module that implements such sitemap provider must register itself to the `SEO` module
by using the `Register` procedure and giving the name of the sitemap file:

```Ada
AWA.SEO.Register ("my-sitemap.xml", My_Provider'Access);
```

Such registration should be made in the `Configure` procedure of the module.

## Integration
To be able to use the `SEO` module, you will need to add the following line in your
GNAT project file:

```Ada
with "awa_seo";
```

The `SEO_Module` type manages the registration of sitemap providers, the registration
of the sitmap servlet and the generation of sitemaps.  Sitemap entries are cached globally
by the module in a protected object.  An instance of the `SEO_Module` must be declared
and registered in the AWA application.  The module instance can be defined as follows:

```Ada
with AWA.SEO.Modules;
...
type Application is new AWA.Applications.Application with record
   SEO_Module : aliased AWA.SEO.Modules.SEO_Module;
end record;
```

And registered in the `Initialize_Modules` procedure by using:

```Ada
Register (App    => App.Self.all'Access,
          Name   => AWA.SEO.Modules.NAME,
          URI    => "seo",
          Module => App.SEO_Module'Access);
```

the `SEO_Module` should be registered before a module that provides a sitemap provider.

## Configuration
The `SEO` module defines the following configuration parameters:

| Name                      | Description                                                    |
|:--------------------------|:---------------------------------------------------------------|
|seo.sitemap_prefix|The URL base prefix to be used for sitemap XML files.|
| |#{app_url_base}/sitemaps/|
|seo.sitemap_refresh|The cache delay in seconds before refreshing the sitemap and querying the sitemap provider.|
| |3600|

## SEO Servlet
The `SEO_Servlet` type is the servlet that generates the sitemap XML
files by using the sitemap providers that have been registered in the
SEO module.  By default, the `AWA.SEO` module provides a configuration
to register that servlet and expose the sitemap URIs but such
configuration can be overriden.  The default configuration is:

```Ada
<servlet-mapping>
    <servlet-name>sitemaps</servlet-name>
    <url-pattern>/sitemaps/*</url-pattern>
</servlet-mapping>
<filter-mapping>
  <filter-name>service</filter-name>
  <url-pattern>/sitemaps/*</url-pattern>
</filter-mapping>
```

