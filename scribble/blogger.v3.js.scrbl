#lang scribble/manual
Hi hi hi
@(require planet/scribble (for-label racket))
@title{Blogger API v3}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
API for access to the data within Blogger.
@hyperlink["https://developers.google.com/blogger/docs/3.0/getting_started" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "blogger.v3.js")]
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

@subsection{pages}
@defproc[(blogger-pages-list
[#:blogId blogId string?]
[#:fetchBodies fetchBodies string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves pages for a blog, possibly filtered.

@racket[blogId]: ID of the blog to fetch pages from.

@racket[fetchBodies]: Whether to retrieve the Page bodies.

}

@defproc[(blogger-pages-get
[#:blogId blogId string?]
[#:pageId pageId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets one blog page by id.

@racket[blogId]: ID of the blog containing the page.

@racket[pageId]: The ID of the page to get.

}

@subsection{posts}
@defproc[(blogger-posts-list
[#:blogId blogId string?]
[#:labels labels string? 'N/A]
[#:endDate endDate string? 'N/A]
[#:fetchBodies fetchBodies string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:startDate startDate string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of posts, possibly filtered.

@racket[blogId]: ID of the blog to fetch posts from.

@racket[labels]: Comma-separated list of labels to search for.

@racket[endDate]: Latest post date to fetch, a date-time with RFC 3339 formatting.

@racket[fetchBodies]: Whether the body content of posts is included.

@racket[maxResults]: Maximum number of posts to fetch.

@racket[pageToken]: Continuation token if the request is paged.

@racket[startDate]: Earliest post date to fetch, a date-time with RFC 3339 formatting.

}

@defproc[(blogger-posts-get
[#:blogId blogId string?]
[#:postId postId string?]
[#:maxComments maxComments string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get a post by id.

@racket[blogId]: ID of the blog to fetch the post from.

@racket[postId]: The ID of the post

@racket[maxComments]: Maximum number of comments to pull back on a post.

}

@defproc[(blogger-posts-insert
[#:blogId blogId string?]
[#:id id string? 'N/A]
[#:location location string? 'N/A]
[#:title title string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:updated updated string? 'N/A]
[#:customMetaData customMetaData string? 'N/A]
[#:published published string? 'N/A]
[#:author author string? 'N/A]
[#:blog blog string? 'N/A]
[#:content content string? 'N/A]
[#:labels labels string? 'N/A]
[#:replies replies string? 'N/A]
[#:url url string? 'N/A]
[#:kind kind string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Add a post.

@racket[blogId]: ID of the blog to fetch the post from.

@racket[id]: The identifier of this Post.

@racket[location]: The location for geotagged posts.

@racket[title]: The title of the Post.

@racket[selfLink]: The API REST URL to fetch this resource from.

@racket[updated]: RFC 3339 date-time when this Post was last updated.

@racket[customMetaData]: The JSON meta-data for the Post.

@racket[published]: RFC 3339 date-time when this Post was published.

@racket[author]: The author of this Post.

@racket[blog]: Data about the blog containing this Post.

@racket[content]: The content of the Post. May contain HTML markup.

@racket[labels]: The list of labels this Post was tagged with.

@racket[replies]: The container of comments on this Post.

@racket[url]: The URL where this Post is displayed.

@racket[kind]: The kind of this entity. Always blogger#post

}

@defproc[(blogger-posts-getByPath
[#:blogId blogId string?]
[#:maxComments maxComments string? 'N/A]
[#:path path string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieve a Post by Path.

@racket[blogId]: ID of the blog to fetch the post from.

@racket[maxComments]: Maximum number of comments to pull back on a post.

@racket[path]: Path of the Post to retrieve.

}

@defproc[(blogger-posts-patch
[#:blogId blogId string?]
[#:postId postId string?]
[#:id id string? 'N/A]
[#:location location string? 'N/A]
[#:title title string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:updated updated string? 'N/A]
[#:customMetaData customMetaData string? 'N/A]
[#:published published string? 'N/A]
[#:author author string? 'N/A]
[#:blog blog string? 'N/A]
[#:content content string? 'N/A]
[#:labels labels string? 'N/A]
[#:replies replies string? 'N/A]
[#:url url string? 'N/A]
[#:kind kind string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update a post. This method supports patch semantics.

@racket[blogId]: The ID of the Blog.

@racket[postId]: The ID of the Post.

@racket[id]: The identifier of this Post.

@racket[location]: The location for geotagged posts.

@racket[title]: The title of the Post.

@racket[selfLink]: The API REST URL to fetch this resource from.

@racket[updated]: RFC 3339 date-time when this Post was last updated.

@racket[customMetaData]: The JSON meta-data for the Post.

@racket[published]: RFC 3339 date-time when this Post was published.

@racket[author]: The author of this Post.

@racket[blog]: Data about the blog containing this Post.

@racket[content]: The content of the Post. May contain HTML markup.

@racket[labels]: The list of labels this Post was tagged with.

@racket[replies]: The container of comments on this Post.

@racket[url]: The URL where this Post is displayed.

@racket[kind]: The kind of this entity. Always blogger#post

}

@defproc[(blogger-posts-search
[#:blogId blogId string?]
[#:q q string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Search for a post.

@racket[blogId]: ID of the blog to fetch the post from.

@racket[q]: Query terms to search this blog for matching posts.

}

@defproc[(blogger-posts-update
[#:blogId blogId string?]
[#:postId postId string?]
[#:id id string? 'N/A]
[#:location location string? 'N/A]
[#:title title string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:updated updated string? 'N/A]
[#:customMetaData customMetaData string? 'N/A]
[#:published published string? 'N/A]
[#:author author string? 'N/A]
[#:blog blog string? 'N/A]
[#:content content string? 'N/A]
[#:labels labels string? 'N/A]
[#:replies replies string? 'N/A]
[#:url url string? 'N/A]
[#:kind kind string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update a post.

@racket[blogId]: The ID of the Blog.

@racket[postId]: The ID of the Post.

@racket[id]: The identifier of this Post.

@racket[location]: The location for geotagged posts.

@racket[title]: The title of the Post.

@racket[selfLink]: The API REST URL to fetch this resource from.

@racket[updated]: RFC 3339 date-time when this Post was last updated.

@racket[customMetaData]: The JSON meta-data for the Post.

@racket[published]: RFC 3339 date-time when this Post was published.

@racket[author]: The author of this Post.

@racket[blog]: Data about the blog containing this Post.

@racket[content]: The content of the Post. May contain HTML markup.

@racket[labels]: The list of labels this Post was tagged with.

@racket[replies]: The container of comments on this Post.

@racket[url]: The URL where this Post is displayed.

@racket[kind]: The kind of this entity. Always blogger#post

}

@defproc[(blogger-posts-delete
[#:blogId blogId string?]
[#:postId postId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Delete a post by id.

@racket[blogId]: The Id of the Blog.

@racket[postId]: The ID of the Post.

}

@subsection{blogs}
@defproc[(blogger-blogs-get
[#:blogId blogId string?]
[#:maxPosts maxPosts string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets one blog by id.

@racket[blogId]: The ID of the blog to get.

@racket[maxPosts]: Maximum number of posts to pull back with the blog.

}

@defproc[(blogger-blogs-getByUrl
[#:url url string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieve a Blog by URL.

@racket[url]: The URL of the blog to retrieve.

}

@defproc[(blogger-blogs-listByUser
[#:userId userId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of blogs, possibly filtered.

@racket[userId]: ID of the user whose blogs are to be fetched. Either the word 'self' (sans quote marks) or the user's profile identifier.

}

@subsection{comments}
@defproc[(blogger-comments-list
[#:blogId blogId string?]
[#:postId postId string?]
[#:endDate endDate string? 'N/A]
[#:fetchBodies fetchBodies string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:startDate startDate string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the comments for a blog, possibly filtered.

@racket[blogId]: ID of the blog to fetch comments from.

@racket[postId]: ID of the post to fetch posts from.

@racket[endDate]: Latest date of comment to fetch, a date-time with RFC 3339 formatting.

@racket[fetchBodies]: Whether the body content of the comments is included.

@racket[maxResults]: Maximum number of comments to include in the result.

@racket[pageToken]: Continuation token if request is paged.

@racket[startDate]: Earliest date of comment to fetch, a date-time with RFC 3339 formatting.

}

@defproc[(blogger-comments-get
[#:blogId blogId string?]
[#:commentId commentId string?]
[#:postId postId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets one comment by id.

@racket[blogId]: ID of the blog to containing the comment.

@racket[commentId]: The ID of the comment to get.

@racket[postId]: ID of the post to fetch posts from.

}

@subsection{users}
@defproc[(blogger-users-get
[#:userId userId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets one user by id.

@racket[userId]: The ID of the user to get.

}

