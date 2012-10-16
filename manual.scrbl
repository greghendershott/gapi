#lang scribble/manual

@(require planet/scribble
          (for-label racket)
          (for-lebel (this-package-in main))
          (for-label (this-package-in dynamic))
          (for-label (this-package-in macro))
          (for-label (this-package-in code-generate))
          )

@title{GAPI: Google API Discovery Service}

@table-of-contents{}

@section{Introduction}


@section{Dynamic}

@defmodule[dynamic]

@defproc[(discovery-document->service [discovery-document jsexpr?]) service?]{

Create a @racket[service?] object representing the web service defined
by the JSON provided as a @racket[jsexpr?].

}


@defproc[(local-discovery-document->service
[filename path-string?]
) service?]{

Create a @racket[service?] object representing the web service defined
by the JSON file @racket[filename].

}


@defproc[(get-discovery-document->service
[name string?]
[ver string?]
) service?]{

Create a @racket[service?] object representing the web service defined
by the name and version, which is fetched via HTTP from the Google API
Discovery service.

}


@defproc[(method-proc
[service service?]
[resource-name symbol?]
[method-name symbol?]
) (dict -> jsexpr?)]{

Given a @racket[service?], @racket[resource-name], and
@racket[method-name], get a procedure that makes HTTP requests to the
service.

The parameters to be supplied to the service are supplied as a
@racket[dict?] where the key is a @racket[symbol?] for the name of the
parameter, and the value is (you guessed it) the value.

The parameter names are those specified by the discovery document.

The response from the service is JSON, and is returned by the function
as a @racket[jsexpr?]. This library doesn't try to marshal the
response into structs. Instead you will need to understand its format
and retrieve the information, for example @racket[(hash-ref js
'items)] if the response contains a list of items.

}


@section{Macro}

@defmodule[macro]

@defform[(require-gapi-doc discovery-document-name)]{

Defines functions respresenting the web service defined by the JSON
file named @racket[discovery-document-name].

Each function makes an HTTP request to the web service. The function
takes normal arguments for parameters that are defined as required,
whereas optional keyword arguments are used to represent other
parameters. These values are supplied in the HTTP request to the service.

The response from the service is JSON, and is returned by the function
as a @racket[jsexpr?]. This library doesn't try to marshal the
response into structs. Instead you will need to understand its format
and retrieve the information, for example @racket[(hash-ref js
'items)] if the response contains a list of items.

As a convenience, the @racket[#:key] argument (which corresponds to
the @tt{key} query parameter in the HTTP request) defaults to the
@racket[api-key] Racket parameter. Put your Google API key in a file
named @tt{.google-api-key} in your home directory--in other words,
@tt{~/.google-api-key}.

As another convenience, see the @racket[paged] form which simplifies
making repeated calls to a service that returns results in small
"pages" (batches of results).

Documentation for the functions is in Scribble-generated HTML files,
one per web service. As I type this, I have no fricking idea how you
will find it, but hopefully I will come back and revise this so you
will know. :)

The currently available service documents are:

@itemize[
@item{@tt{adexchangebuyer.v1.1.js}: Lets you manage your Ad Exchange Buyer account.}
@item{@tt{adsense.v1.1.js}: Gives AdSense publishers access to their inventory and the ability to generate reports}
@item{@tt{adsensehost.v4.1.js}: Gives AdSense Hosts access to report generation, ad code generation, and publisher management capabilities.}
@item{@tt{analytics.v3.js}: View and manage your Google Analytics data}
@item{@tt{androidpublisher.v1.js}: Lets Android application developers access their Google Play accounts.}
@item{@tt{audit.v1.js}: Lets you access user activities in your enterprise made through various applications.}
@item{@tt{bigquery.v2.js}: A data platform for customers to create, manage, share and query data.}
@item{@tt{blogger.v3.js}: API for access to the data within Blogger.}
@item{@tt{books.v1.js}: Lets you search for books and manage your Google Books library.}
@item{@tt{calendar.v3.js}: Lets you manipulate events and other calendar data.}
@item{@tt{civicinfo.us_v1.js}: An API for accessing civic information.}
@item{@tt{compute.v1beta12.js}: API for the Google Compute Engine service.}
@item{@tt{coordinate.v1.js}: Lets you view and manage jobs in a Coordinate team.}
@item{@tt{customsearch.v1.js}: Lets you search over a website or collection of websites}
@item{@tt{dfareporting.v1.1.js}: Lets you create, run and download reports.}
@item{@tt{discovery.v1.js}: Lets you discover information about other Google APIs, such as what APIs are available, the resource and method details for each API}
@item{@tt{drive.v2.js}: The API to interact with Drive.}
@item{@tt{freebase.v1.js}: Lets you access the Freebase repository of open data.}
@item{@tt{fusiontables.v1.js}: API for working with Fusion Tables data.}
@item{@tt{gan.v1beta1.js}: Lets you have programmatic access to your Google Affiliate Network data.}
@item{@tt{groupssettings.v1.js}: Lets you manage permission levels and related settings of a group.}
@item{@tt{latitude.v1.js}: Lets you read and update your current location and work with your location history}
@item{@tt{licensing.v1.js}: Licensing API to view and manage license for your domain.}
@item{@tt{moderator.v1.js}: Moderator API}
@item{@tt{oauth2.v2.js}: Lets you access OAuth2 protocol related APIs.}
@item{@tt{orkut.v2.js}: Lets you manage activities, comments and badges in Orkut. More stuff coming in time.}
@item{@tt{pagespeedonline.v1.js}: Lets you analyze the performance of a web page and get tailored suggestions to make that page faster.}
@item{@tt{plus.v1.js}: The Google+ API enables developers to build on top of the Google+ platform.}
@item{@tt{prediction.v1.5.js}: Lets you access a cloud hosted machine learning service that makes it easy to build smart apps}
@item{@tt{reseller.v1.js}: Lets you create and manage your customers and their subscriptions.}
@item{@tt{shopping.v1.js}: Lets you search over product data.}
@item{@tt{siteVerification.v1.js}: Lets you programatically verify ownership of websites or domains with Google.}
@item{@tt{storage.v1beta1.js}: Lets you store and retrieve potentially-large, immutable data objects.}
@item{@tt{taskqueue.v1beta2.js}: Lets you access a Google App Engine Pull Task Queue over REST.}
@item{@tt{tasks.v1.js}: Lets you manage your tasks and task lists.}
@item{@tt{translate.v2.js}: Lets you translate text from one language to another}
@item{@tt{urlshortener.v1.js}: Lets you create, inspect, and manage goo.gl short URLs}
@item{@tt{webfonts.v1.js}: The Google Web Fonts Developer API.}
@item{@tt{youtube.v3.js}: Programmatic access to YouTube features.}
@item{@tt{youtubeAnalytics.v1.js}: Retrieve your YouTube Analytics reports.}
]

}


@defform[(paged (function arguments ...))]{

Makes repeated calls to @racket[function] with @racket[arguments] if
necessary to obtain additional results. If a set of results from the
web service includes a @tt{nextPageToken} value, indicating that more
results are available, then @racket[function] is called again with
@racket[arguments] plus the additional argument @racket[#:pageToken].

If a service will return a very large amount of results, you may
@italic{want} to make the calls individually and process the results
in small batches.  However if you would rather get the all of the
results in one, albeit potentially very large, list of items, use this
form.

}

@section{Generating .RKT files}

@defmodule[code-generate]
