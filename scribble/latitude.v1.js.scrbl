#lang scribble/manual
Hi hi hi
@(require planet/scribble (for-label racket))
@title{Google Latitude API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you read and update your current location and work with your location history
@hyperlink["https://developers.google.com/latitude/v1/using" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "latitude.v1.js")]
@section{API Parameters}
The following optional keyword arguments may be passed to all functions for this web service:
@defproc[(_
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]

) jsexpr?]{
@margin-note{This is not actually a function. This is just using Scribble's defproc form to list the optional keyword arguments that may be passed to @italic{all} functions for this service.}
@racket[fields]: Selector specifying which fields to include in a partial response.

@racket[key]: API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.

@racket[alt]: Data format for the response.

@racket[oauth_token]: OAuth 2.0 token for the current user.

@racket[prettyPrint]: Returns response with indentations and line breaks.

@racket[quotaUser]: Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.

@racket[userIp]: IP address of the site where the request originates. Use this if you want to enforce per-user limits.


}
@section{Resources}

@subsection{location}
@defproc[(latitude-location-list
[#:granularity granularity string? 'N/A]
[#:max-results max-results string? 'N/A]
[#:max-time max-time string? 'N/A]
[#:min-time min-time string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists the user's location history.

@racket[granularity]: Granularity of the requested locations.

@racket[max-results]: Maximum number of locations to return.

@racket[max-time]: Maximum timestamp of locations to return (ms since epoch).

@racket[min-time]: Minimum timestamp of locations to return (ms since epoch).

}

@defproc[(latitude-location-get
[#:locationId locationId string?]
[#:granularity granularity string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Reads a location from the user's location history.

@racket[locationId]: Timestamp of the location to read (ms since epoch).

@racket[granularity]: Granularity of the location to return.

}

@defproc[(latitude-location-insert
[#:kind kind string? 'N/A]
[#:accuracy accuracy string? 'N/A]
[#:activityId activityId string? 'N/A]
[#:altitude altitude string? 'N/A]
[#:altitudeAccuracy altitudeAccuracy string? 'N/A]
[#:heading heading string? 'N/A]
[#:latitude latitude string? 'N/A]
[#:longitude longitude string? 'N/A]
[#:speed speed string? 'N/A]
[#:timestampMs timestampMs string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts or updates a location in the user's location history.

@racket[kind]: Kind of this item.

@racket[accuracy]: Accuracy of the latitude and longitude coordinates, in non-negative meters. Optional.

@racket[activityId]: Unique ID of the Buzz message that corresponds to the check-in associated with this location. Available only for check-in locations. Optional.

@racket[altitude]: Altitude of the location, in meters. Optional.

@racket[altitudeAccuracy]: Accuracy of the altitude value, in meters. Optional.

@racket[heading]: Direction of travel of the user when this location was recorded. In degrees, clockwise relative to true north. Optional.

@racket[latitude]: Latitude of the location, in decimal degrees.

@racket[longitude]: Longitude of the location, in decimal degrees.

@racket[speed]: Ground speed of the user at the time this location was recorded, in meters per second. Non-negative. Optional.

@racket[timestampMs]: Timestamp of the Location Resource, in milliseconds since the epoch (UTC). This is also the Location Resource's unique id.

}

@defproc[(latitude-location-delete
[#:locationId locationId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes a location from the user's location history.

@racket[locationId]: Timestamp of the location to delete (ms since epoch).

}

@subsection{currentLocation}
@defproc[(latitude-currentLocation-get
[#:granularity granularity string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the authenticated user's current location.

@racket[granularity]: Granularity of the requested location.

}

@defproc[(latitude-currentLocation-insert
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates or creates the user's current location.

}

@defproc[(latitude-currentLocation-delete
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the authenticated user's current location.

}

