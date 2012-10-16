#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Orkut API v2}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you manage activities, comments and badges in Orkut. More stuff coming in time.
@hyperlink["http://code.google.com/apis/orkut/v2/reference.html" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "orkut.v2.js")]
@section{API Parameters}
The following optional keyword arguments may be passed to @italic{all} functions for this web service:
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

@subsection{activities}
@defproc[(orkut.activities.list
[collection string?]
[userId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of activities.

@racket[collection]: The collection of activities to list.

@racket[userId]: The ID of the user whose activities will be listed. Can be me to refer to the viewer (i.e. the authenticated user).

@racket[maxResults]: The maximum number of activities to include in the response.

@racket[pageToken]: A continuation token that allows pagination.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@defproc[(orkut.activities.delete
[activityId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes an existing activity, if the access controls allow it.

@racket[activityId]: ID of the activity to remove.

}

@subsection{activityVisibility}
@defproc[(orkut.activityVisibility.get
[activityId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets the visibility of an existing activity.

@racket[activityId]: ID of the activity to get the visibility.

}

@defproc[(orkut.activityVisibility.patch
[activityId string?]
[#:kind kind string? 'N/A]
[#:visibility visibility string? 'N/A]
[#:links links string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the visibility of an existing activity. This method supports patch semantics.

@racket[activityId]: ID of the activity.

@racket[kind]: Identifies this resource as a visibility item. Value: "orkut#visibility"

@racket[visibility]: The visibility of the resource. Possible values are:  
- default: not hidden by the user 
- hidden: hidden

@racket[links]: List of resources for the visibility item.

}

@defproc[(orkut.activityVisibility.update
[activityId string?]
[#:kind kind string? 'N/A]
[#:visibility visibility string? 'N/A]
[#:links links string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the visibility of an existing activity.

@racket[activityId]: ID of the activity.

@racket[kind]: Identifies this resource as a visibility item. Value: "orkut#visibility"

@racket[visibility]: The visibility of the resource. Possible values are:  
- default: not hidden by the user 
- hidden: hidden

@racket[links]: List of resources for the visibility item.

}

@subsection{badges}
@defproc[(orkut.badges.list
[userId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the list of visible badges of a user.

@racket[userId]: The id of the user whose badges will be listed. Can be me to refer to caller.

}

@defproc[(orkut.badges.get
[badgeId string?]
[userId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a badge from a user.

@racket[badgeId]: The ID of the badge that will be retrieved.

@racket[userId]: The ID of the user whose badges will be listed. Can be me to refer to caller.

}

@subsection{comments}
@defproc[(orkut.comments.list
[activityId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:hl hl string? 'N/A]
[#:orderBy orderBy string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of comments, possibly filtered.

@racket[activityId]: The ID of the activity containing the comments.

@racket[maxResults]: The maximum number of activities to include in the response.

@racket[pageToken]: A continuation token that allows pagination.

@racket[hl]: Specifies the interface language (host language) of your user interface.

@racket[orderBy]: Sort search results.

}

@defproc[(orkut.comments.get
[commentId string?]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves an existing comment.

@racket[commentId]: ID of the comment to get.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@defproc[(orkut.comments.insert
[activityId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:content content string? 'N/A]
[#:actor actor string? 'N/A]
[#:published published string? 'N/A]
[#:inReplyTo inReplyTo string? 'N/A]
[#:links links string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a new comment to an activity.

@racket[activityId]: The ID of the activity to contain the new comment.

@racket[id]: The unique ID for the comment.

@racket[kind]: Identifies this resource as a comment. Value: "orkut#comment"

@racket[content]: The content of the comment in text/html

@racket[actor]: The person who posted the comment.

@racket[published]: The time the comment was initially published, in RFC 3339 format.

@racket[inReplyTo]: Link to the original activity where this comment was posted.

@racket[links]: List of resources for the comment.

}

@defproc[(orkut.comments.delete
[commentId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes an existing comment.

@racket[commentId]: ID of the comment to remove.

}

@subsection{communities}
@defproc[(orkut.communities.list
[userId string?]
[#:maxResults maxResults string? 'N/A]
[#:hl hl string? 'N/A]
[#:orderBy orderBy string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the list of communities the current user is a member of.

@racket[userId]: The ID of the user whose communities will be listed. Can be me to refer to caller.

@racket[maxResults]: The maximum number of communities to include in the response.

@racket[hl]: Specifies the interface language (host language) of your user interface.

@racket[orderBy]: How to order the communities by.

}

@defproc[(orkut.communities.get
[communityId string?]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the basic information (aka. profile) of a community.

@racket[communityId]: The ID of the community to get.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@subsection{communityFollow}
@defproc[(orkut.communityFollow.insert
[userId string?]
[communityId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Adds a user as a follower of a community.

@racket[userId]: ID of the user.

@racket[communityId]: ID of the community.

}

@defproc[(orkut.communityFollow.delete
[userId string?]
[communityId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Removes a user from the followers of a community.

@racket[userId]: ID of the user.

@racket[communityId]: ID of the community.

}

@subsection{communityMembers}
@defproc[(orkut.communityMembers.list
[communityId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:hl hl string? 'N/A]
[#:friendsOnly friendsOnly string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists members of a community. Use the pagination tokens to retrieve the full list; do not rely on the member count available in the community profile information to know when to stop iterating, as that count may be approximate.

@racket[communityId]: The ID of the community whose members will be listed.

@racket[maxResults]: The maximum number of members to include in the response.

@racket[pageToken]: A continuation token that allows pagination.

@racket[hl]: Specifies the interface language (host language) of your user interface.

@racket[friendsOnly]: Whether to list only community members who are friends of the user.

}

@defproc[(orkut.communityMembers.get
[userId string?]
[communityId string?]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the relationship between a user and a community.

@racket[userId]: ID of the user.

@racket[communityId]: ID of the community.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@defproc[(orkut.communityMembers.insert
[userId string?]
[communityId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Makes the user join a community.

@racket[userId]: ID of the user.

@racket[communityId]: ID of the community.

}

@defproc[(orkut.communityMembers.delete
[userId string?]
[communityId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Makes the user leave a community.

@racket[userId]: ID of the user.

@racket[communityId]: ID of the community.

}

@subsection{communityMessages}
@defproc[(orkut.communityMessages.list
[topicId string?]
[communityId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the messages of a topic of a community.

@racket[topicId]: The ID of the topic which messages will be listed.

@racket[communityId]: The ID of the community which messages will be listed.

@racket[maxResults]: The maximum number of messages to include in the response.

@racket[pageToken]: A continuation token that allows pagination.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@defproc[(orkut.communityMessages.insert
[topicId string?]
[communityId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:body body string? 'N/A]
[#:links links string? 'N/A]
[#:addedDate addedDate string? 'N/A]
[#:isSpam isSpam string? 'N/A]
[#:subject subject string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Adds a message to a given community topic.

@racket[topicId]: The ID of the topic the message should be added to.

@racket[communityId]: The ID of the community the message should be added to.

@racket[id]: The ID of the message.

@racket[kind]: Identifies this resource as a community message. Value: "orkut#communityMessage"

@racket[author]: The creator of the message. If ommited, the message is annonimous.

@racket[body]: The body of the message.

@racket[links]: List of resources for the community message.

@racket[addedDate]: The timestamp of the date when the message was added, in RFC 3339 format.

@racket[isSpam]: Whether this post was marked as spam by the viewer, when he/she is not the community owner or one of its moderators.

@racket[subject]: The subject of the message.

}

@defproc[(orkut.communityMessages.delete
[messageId string?]
[topicId string?]
[communityId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Moves a message of the community to the trash folder.

@racket[messageId]: The ID of the message to be moved to the trash folder.

@racket[topicId]: The ID of the topic whose message will be moved to the trash folder.

@racket[communityId]: The ID of the community whose message will be moved to the trash folder.

}

@subsection{communityPollComments}
@defproc[(orkut.communityPollComments.list
[pollId string?]
[communityId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the comments of a community poll.

@racket[pollId]: The ID of the community whose polls will be listed.

@racket[communityId]: The ID of the community whose poll is having its comments listed.

@racket[maxResults]: The maximum number of comments to include in the response.

@racket[pageToken]: A continuation token that allows pagination.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@defproc[(orkut.communityPollComments.insert
[pollId string?]
[communityId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:body body string? 'N/A]
[#:addedDate addedDate string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Adds a comment on a community poll.

@racket[pollId]: The ID of the poll being commented.

@racket[communityId]: The ID of the community whose poll is being commented.

@racket[id]: The ID of the comment.

@racket[kind]: Identifies this resource as a community poll comment. Value: "orkut#communityPollComment"

@racket[author]: The creator of the comment.

@racket[body]: The body of the message.

@racket[addedDate]: The date when the message was added, in RFC 3339 format.

}

@subsection{communityPollVotes}
@defproc[(orkut.communityPollVotes.insert
[pollId string?]
[communityId string?]
[#:kind kind string? 'N/A]
[#:isVotevisible isVotevisible string? 'N/A]
[#:optionIds optionIds string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Votes on a community poll.

@racket[pollId]: The ID of the poll being voted.

@racket[communityId]: The ID of the community whose poll is being voted.

@racket[kind]: Identifies this resource as a community poll vote. Value: "orkut#communityPollVote"

@racket[isVotevisible]: Whether this vote is visible to other users or not.

@racket[optionIds]: The ids of the voted options.

}

@subsection{communityPolls}
@defproc[(orkut.communityPolls.list
[communityId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the polls of a community.

@racket[communityId]: The ID of the community which polls will be listed.

@racket[maxResults]: The maximum number of polls to include in the response.

@racket[pageToken]: A continuation token that allows pagination.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@defproc[(orkut.communityPolls.get
[pollId string?]
[communityId string?]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves one specific poll of a community.

@racket[pollId]: The ID of the poll to get.

@racket[communityId]: The ID of the community for whose poll will be retrieved.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@subsection{communityRelated}
@defproc[(orkut.communityRelated.list
[communityId string?]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the communities related to another one.

@racket[communityId]: The ID of the community whose related communities will be listed.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@subsection{communityTopics}
@defproc[(orkut.communityTopics.list
[communityId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the topics of a community.

@racket[communityId]: The ID of the community which topics will be listed.

@racket[maxResults]: The maximum number of topics to include in the response.

@racket[pageToken]: A continuation token that allows pagination.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@defproc[(orkut.communityTopics.get
[topicId string?]
[communityId string?]
[#:hl hl string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a topic of a community.

@racket[topicId]: The ID of the topic to get.

@racket[communityId]: The ID of the community whose topic will be retrieved.

@racket[hl]: Specifies the interface language (host language) of your user interface.

}

@defproc[(orkut.communityTopics.insert
[communityId string?]
[#:isShout isShout string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:author author string? 'N/A]
[#:body body string? 'N/A]
[#:isClosed isClosed string? 'N/A]
[#:lastUpdate lastUpdate string? 'N/A]
[#:latestMessageSnippet latestMessageSnippet string? 'N/A]
[#:messages messages string? 'N/A]
[#:numberOfReplies numberOfReplies string? 'N/A]
[#:links links string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Adds a topic to a given community.

@racket[communityId]: The ID of the community the topic should be added to.

@racket[isShout]: Whether this topic is a shout.

@racket[id]: The ID of the topic.

@racket[kind]: Identifies this resource as a community topic. Value: "orkut#communityTopic"

@racket[title]: The title of the topic.

@racket[author]: The creator of the topic.

@racket[body]: The body of the topic.

@racket[isClosed]: Whether the topic is closed for new messages.

@racket[lastUpdate]: The timestamp of the last update, in RFC 3339 format.

@racket[latestMessageSnippet]: Snippet of the last message posted on this topic.

@racket[messages]: Most recent messages.

@racket[numberOfReplies]: The total number of replies this topic has received.

@racket[links]: List of resources for the community.

}

@defproc[(orkut.communityTopics.delete
[topicId string?]
[communityId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Moves a topic of the community to the trash folder.

@racket[topicId]: The ID of the topic to be moved to the trash folder.

@racket[communityId]: The ID of the community whose topic will be moved to the trash folder.

}

@subsection{scraps}
@defproc[(orkut.scraps.insert
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:object object string? 'N/A]
[#:updated updated string? 'N/A]
[#:actor actor string? 'N/A]
[#:access access string? 'N/A]
[#:published published string? 'N/A]
[#:verb verb string? 'N/A]
[#:links links string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new scrap.

@racket[id]: The ID for the activity.

@racket[kind]: The kind of activity. Always orkut#activity.

@racket[title]: Title of the activity.

@racket[object]: The activity's object.

@racket[updated]: The time at which the activity was last updated.

@racket[actor]: The person who performed the activity.

@racket[access]: Identifies who has access to see this activity.

@racket[published]: The time at which the activity was initially published.

@racket[verb]: This activity's verb, indicating what action was performed. Possible values are:  
- add - User added new content to profile or album, e.g. video, photo. 
- post - User publish content to the stream, e.g. status, scrap. 
- update - User commented on an activity. 
- make-friend - User added a new friend. 
- birthday - User has a birthday.

@racket[links]: Links to resources related to this activity.

}

@subsection{acl}
@defproc[(orkut.acl.delete
[activityId string?]
[userId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Excludes an element from the ACL of the activity.

@racket[activityId]: ID of the activity.

@racket[userId]: ID of the user to be removed from the activity.

}

@subsection{counters}
@defproc[(orkut.counters.list
[userId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the counters of a user.

@racket[userId]: The ID of the user whose counters will be listed. Can be me to refer to caller.

}

