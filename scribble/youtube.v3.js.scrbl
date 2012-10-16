#lang scribble/manual
Hi hi hi
@(require planet/scribble (for-label racket))
@title{YouTube API v3}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Programmatic access to YouTube features.
@hyperlink["https://developers.google.com/youtube" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "youtube.v3.js")]
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

@subsection{channels}
@defproc[(youtube-channels-list
[#:part part string?]
[#:id id string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:categoryId categoryId string? 'N/A]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:mine mine string? 'N/A]
[#:mySubscribers mySubscribers string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Browse the YouTube channel collection. Either the 'id' or 'mine' parameter must be set.

@racket[part]: One or more parts to return on the current request.

@racket[id]: YouTube IDs of the channels to be returned.

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Token for the page selection.

@racket[categoryId]: Filter to retrieve the channels within the given category ID.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[mine]: Filter to only channels owned by authenticated user.

@racket[mySubscribers]: Filter to channels that subscribed to the channel of the authenticated user.

}

@subsection{subscription}
@defproc[(youtube-subscription-list
[#:part part string?]
[#:id id string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:channelId channelId string? 'N/A]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:mine mine string? 'N/A]
[#:order order string? 'N/A]
[#:forChannelId forChannelId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Browse user's subscription collection.

@racket[part]: One or more parts to return on the current request.

@racket[id]: YouTube IDs of the subscriptions to be returned.

@racket[maxResults]: Maximum number of search results to return per page.

@racket[pageToken]: Token for the page selection.

@racket[channelId]: Only return subscriptions to given channelId.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[mine]: Flag indicating only return the subscriptions of the authenticated user.

@racket[order]: Sort order.

@racket[forChannelId]: Takes a comma separated list of channel IDs. Filters the returned list to only those matching these channels

}

@defproc[(youtube-subscription-insert
[#:part part string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:id id string? 'N/A]
[#:etag etag string? 'N/A]
[#:kind kind string? 'N/A]
[#:contentDetails contentDetails string? 'N/A]
[#:snippet snippet string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Insert a subscription.

@racket[part]: One or more parts to return on the current request.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[id]: The unique id of the subscription.

@racket[etag]: The eTag of the subscription.

@racket[kind]: The type of this API resource.

@racket[contentDetails]: Basic statistics about the subscription

@racket[snippet]: Basic details about the subscription

}

@defproc[(youtube-subscription-delete
[#:id id string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes subscriptions by IDs.

@racket[id]: YouTube IDs of the subscription to be deleted.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

}

@subsection{activities}
@defproc[(youtube-activities-list
[#:part part string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:channelId channelId string? 'N/A]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:home home string? 'N/A]
[#:mine mine string? 'N/A]
[#:publishedAfter publishedAfter string? 'N/A]
[#:publishedBefore publishedBefore string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Browse the YouTube channel activity collection.

@racket[part]: One or more parts to return on the current request.

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Token for the page selection.

@racket[channelId]: YouTube ID of the channel.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[home]: Flag indicating to return user's homepage feed.

@racket[mine]: Flag indicating to return user's activities.

@racket[publishedAfter]: Only return activities published after given date (inclusive).

@racket[publishedBefore]: Only return activities published before given date (exclusive).

}

@defproc[(youtube-activities-insert
[#:part part string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:id id string? 'N/A]
[#:etag etag string? 'N/A]
[#:kind kind string? 'N/A]
[#:contentDetails contentDetails string? 'N/A]
[#:snippet snippet string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Post a channel bulletin.

@racket[part]: One or more parts to return on the current request.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[id]: The unique ID of the activity.

@racket[etag]: The eTag of the activity.

@racket[kind]: The type of this API response.

@racket[contentDetails]: Type specific information about the activity.

@racket[snippet]: Basic details about the activity: title, description, thumbnails.

}

@subsection{channelCategories}
@defproc[(youtube-channelCategories-list
[#:part part string?]
[#:id id string? 'N/A]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:hl hl string? 'N/A]
[#:regionCode regionCode string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Browse the YouTube guide category collection.

@racket[part]: One or more parts to return on the current request.

@racket[id]: Comma-separated YouTube IDs of the channelCategories to be returned.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[hl]: Language for the returned channelCategories.

@racket[regionCode]: Return the channelCategories in the given region code.

}

@subsection{playlistItems}
@defproc[(youtube-playlistItems-list
[#:part part string?]
[#:id id string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:playlistId playlistId string? 'N/A]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Browse the YouTube playlist collection.

@racket[part]: One or more parts to return on the current request.

@racket[id]: YouTube IDs of the playlist items to be returned.

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Token for the page selection.

@racket[playlistId]: Retrieves playlist items from the given playlist id.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

}

@defproc[(youtube-playlistItems-insert
[#:part part string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:id id string? 'N/A]
[#:etag etag string? 'N/A]
[#:kind kind string? 'N/A]
[#:contentDetails contentDetails string? 'N/A]
[#:snippet snippet string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Insert a resource into a playlist.

@racket[part]: One or more parts to return on the current request.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[id]: The unique id of the playlist item.

@racket[etag]: The eTag of the playlist item.

@racket[kind]: The type of this API resource.

@racket[contentDetails]: Content details about the playlist item: start and end clipping time.

@racket[snippet]: Basic details about the playlist item: title, description, thumbnails.

}

@defproc[(youtube-playlistItems-update
[#:part part string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:id id string? 'N/A]
[#:etag etag string? 'N/A]
[#:kind kind string? 'N/A]
[#:contentDetails contentDetails string? 'N/A]
[#:snippet snippet string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update a playlist item.

@racket[part]: One or more parts to return on the current request.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[id]: The unique id of the playlist item.

@racket[etag]: The eTag of the playlist item.

@racket[kind]: The type of this API resource.

@racket[contentDetails]: Content details about the playlist item: start and end clipping time.

@racket[snippet]: Basic details about the playlist item: title, description, thumbnails.

}

@defproc[(youtube-playlistItems-delete
[#:id id string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes playlist items by IDs.

@racket[id]: YouTube IDs of the playlist items to be deleted.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

}

@subsection{playlists}
@defproc[(youtube-playlists-list
[#:part part string?]
[#:id id string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:mine mine string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Browse the YouTube playlist collection.

@racket[part]: One or more parts to return on the current request.

@racket[id]: Comma-separated YouTube IDs of the playlists to be returned.

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Token for the page selection.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[mine]: Flag indicating only return the playlists of the authenticated user.

}

@defproc[(youtube-playlists-insert
[#:part part string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:id id string? 'N/A]
[#:etag etag string? 'N/A]
[#:status status string? 'N/A]
[#:kind kind string? 'N/A]
[#:snippet snippet string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Create a playlist.

@racket[part]: One or more parts to return on the current request.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[id]: The unique id of the playlist.

@racket[etag]: The eTag of the playlist.

@racket[status]: Status of the playlist: only privacy_status for now.

@racket[kind]: The type of this API resource.

@racket[snippet]: Basic details about the playlist: title, description, thumbnails.

}

@defproc[(youtube-playlists-update
[#:part part string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:id id string? 'N/A]
[#:etag etag string? 'N/A]
[#:status status string? 'N/A]
[#:kind kind string? 'N/A]
[#:snippet snippet string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update a playlist.

@racket[part]: One or more parts to return on the current request.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[id]: The unique id of the playlist.

@racket[etag]: The eTag of the playlist.

@racket[status]: Status of the playlist: only privacy_status for now.

@racket[kind]: The type of this API resource.

@racket[snippet]: Basic details about the playlist: title, description, thumbnails.

}

@defproc[(youtube-playlists-delete
[#:id id string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes playlists by IDs.

@racket[id]: YouTube IDs of the playlists to be deleted.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

}

@subsection{videos}
@defproc[(youtube-videos-list
[#:id id string?]
[#:part part string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Browse the YouTube video collection.

@racket[id]: YouTube IDs of the videos to be returned.

@racket[part]: One or more parts to return on the current request.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

}

@defproc[(youtube-videos-insert
[#:part part string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:id id string? 'N/A]
[#:etag etag string? 'N/A]
[#:status status string? 'N/A]
[#:kind kind string? 'N/A]
[#:contentDetails contentDetails string? 'N/A]
[#:snippet snippet string? 'N/A]
[#:statistics statistics string? 'N/A]
[#:player player string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Upload a video to YouTube.

@racket[part]: One or more parts to return on the current request.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[id]: The unique id of the video.

@racket[etag]: The eTag of the video.

@racket[status]: Status of the video upload, privacy status.

@racket[kind]: The type of this API resource.

@racket[contentDetails]: Information about the video content, media file.

@racket[snippet]: Basic details about the video: title, description, thumbnails.

@racket[statistics]: Statistics about the video: number of views, ratings.

@racket[player]: Information used to play the video.

}

@defproc[(youtube-videos-update
[#:part part string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:id id string? 'N/A]
[#:etag etag string? 'N/A]
[#:status status string? 'N/A]
[#:kind kind string? 'N/A]
[#:contentDetails contentDetails string? 'N/A]
[#:snippet snippet string? 'N/A]
[#:statistics statistics string? 'N/A]
[#:player player string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update a video.

@racket[part]: One or more parts to return on the current request.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[id]: The unique id of the video.

@racket[etag]: The eTag of the video.

@racket[status]: Status of the video upload, privacy status.

@racket[kind]: The type of this API resource.

@racket[contentDetails]: Information about the video content, media file.

@racket[snippet]: Basic details about the video: title, description, thumbnails.

@racket[statistics]: Statistics about the video: number of views, ratings.

@racket[player]: Information used to play the video.

}

@defproc[(youtube-videos-delete
[#:id id string?]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Delete a YouTube video.

@racket[id]: YouTube ID of the video to be deleted.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

}

@subsection{search}
@defproc[(youtube-search-list
[#:part part string?]
[#:type type string? 'N/A]
[#:published published string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:topicId topicId string? 'N/A]
[#:q q string? 'N/A]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:order order string? 'N/A]
[#:relatedToVideo relatedToVideo string? 'N/A]
[#:videoCaption videoCaption string? 'N/A]
[#:videoDefinition videoDefinition string? 'N/A]
[#:videoDimension videoDimension string? 'N/A]
[#:videoDuration videoDuration string? 'N/A]
[#:videoLicense videoLicense string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Universal search for youtube.

@racket[part]: One or more parts to return on the current request.

@racket[type]: Type of resource to search.

@racket[published]: Only search for resources uploaded at a specific pediod

@racket[maxResults]: Maximum number of search results to return per page.

@racket[pageToken]: Token for the page selection.

@racket[topicId]: Only search for resources with the specified topic

@racket[q]: Query to search in Youtube.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[order]: Sort order.

@racket[relatedToVideo]: Search for resources related to this video. Need to be used with type set to 'video'

@racket[videoCaption]: Add a filter on the the presence of captions on the videos.

@racket[videoDefinition]: Add a filter for the definition of the videos.

@racket[videoDimension]: Add a filter for the number of dimensions in the videos.

@racket[videoDuration]: Add a filter on the duration of the videos.

@racket[videoLicense]: Add a filter on the licensing of the videos.

}

@subsection{videocategory}
@defproc[(youtube-videocategory-list
[#:part part string?]
[#:id id string? 'N/A]
[#:contentOwnerId contentOwnerId string? 'N/A]
[#:hl hl string? 'N/A]
[#:regionCode regionCode string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Browse list of video categories.

@racket[part]: One or more parts to return on the current request.

@racket[id]: IDs of the categories to be returned.

@racket[contentOwnerId]: The authenticated user acts on behalf of this content owner.

@racket[hl]: Language used for the title of the categories.

@racket[regionCode]: Return all the categories in this region.

}

