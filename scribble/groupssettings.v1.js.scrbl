#lang scribble/manual
@title{Groups Settings API v1}
Lets you manage permission levels and related settings of a group.
@hyperlink["https://developers.google.com/google-apps/groups-settings/get_started" "Documentation link"]
@table-of-contents{}
@section{API Parameters}
These optional keyword arguments may be passed to all functions for this API:
@defproc[(any-function
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
This is not actually a function. This is just using Scribble's
defproc form to list the optional keyword arguments that may be passed
to any function for this API.

@racket[fields]: Selector specifying which fields to include in a partial response.

@racket[key]: API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.

@racket[alt]: Data format for the response.

@racket[oauth_token]: OAuth 2.0 token for the current user.

@racket[prettyPrint]: Returns response with indentations and line breaks.

@racket[quotaUser]: Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.

@racket[userIp]: IP address of the site where the request originates. Use this if you want to enforce per-user limits.

}


@section{Functions for the `groups' resource}
@defproc[(groupsSettings.groups.get
[groupUniqueId string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets one resource by id.

@racket[groupUniqueId]: The resource ID

}

@defproc[(groupsSettings.groups.patch
[groupUniqueId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:email email string? 'N/A]
[#:allowExternalMembers allowExternalMembers string? 'N/A]
[#:allowGoogleCommunication allowGoogleCommunication string? 'N/A]
[#:allowWebPosting allowWebPosting string? 'N/A]
[#:archiveOnly archiveOnly string? 'N/A]
[#:customReplyTo customReplyTo string? 'N/A]
[#:defaultMessageDenyNotificationText defaultMessageDenyNotificationText string? 'N/A]
[#:includeInGlobalAddressList includeInGlobalAddressList string? 'N/A]
[#:isArchived isArchived string? 'N/A]
[#:maxMessageBytes maxMessageBytes string? 'N/A]
[#:membersCanPostAsTheGroup membersCanPostAsTheGroup string? 'N/A]
[#:messageDisplayFont messageDisplayFont string? 'N/A]
[#:messageModerationLevel messageModerationLevel string? 'N/A]
[#:primaryLanguage primaryLanguage string? 'N/A]
[#:replyTo replyTo string? 'N/A]
[#:sendMessageDenyNotification sendMessageDenyNotification string? 'N/A]
[#:showInGroupDirectory showInGroupDirectory string? 'N/A]
[#:spamModerationLevel spamModerationLevel string? 'N/A]
[#:whoCanInvite whoCanInvite string? 'N/A]
[#:whoCanJoin whoCanJoin string? 'N/A]
[#:whoCanPostMessage whoCanPostMessage string? 'N/A]
[#:whoCanViewGroup whoCanViewGroup string? 'N/A]
[#:whoCanViewMembership whoCanViewMembership string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing resource. This method supports patch semantics.

@racket[groupUniqueId]: The resource ID

@racket[kind]: The type of the resource.

@racket[name]: Name of the Group

@racket[description]: Description of the group

@racket[email]: Email id of the group

@racket[allowExternalMembers]: Are external members allowed to join the group.

@racket[allowGoogleCommunication]: Is google allowed to contact admins.

@racket[allowWebPosting]: If posting from web is allowed.

@racket[archiveOnly]: If the group is archive only

@racket[customReplyTo]: Default email to which reply to any message should go.

@racket[defaultMessageDenyNotificationText]: Default message deny notification message

@racket[includeInGlobalAddressList]: If this groups should be included in global address list or not.

@racket[isArchived]: If the contents of the group are archived.

@racket[maxMessageBytes]: Maximum message size allowed.

@racket[membersCanPostAsTheGroup]: Can members post using the group email address.

@racket[messageDisplayFont]: Default message display font. Possible values are: DEFAULT_FONT FIXED_WIDTH_FONT

@racket[messageModerationLevel]: Moderation level for messages. Possible values are: MODERATE_ALL_MESSAGES MODERATE_NON_MEMBERS MODERATE_NEW_MEMBERS MODERATE_NONE

@racket[primaryLanguage]: Primary language for the group.

@racket[replyTo]: Whome should the default reply to a message go to. Possible values are: REPLY_TO_CUSTOM REPLY_TO_SENDER REPLY_TO_LIST REPLY_TO_OWNER REPLY_TO_IGNORE REPLY_TO_MANAGERS

@racket[sendMessageDenyNotification]: Should the member be notified if his message is denied by owner.

@racket[showInGroupDirectory]: Is the group listed in groups directory

@racket[spamModerationLevel]: Moderation level for messages detected as spam. Possible values are: ALLOW MODERATE SILENTLY_MODERATE REJECT

@racket[whoCanInvite]: Permissions to invite members. Possbile values are: ALL_MEMBERS_CAN_INVITE ALL_MANAGERS_CAN_INVITE

@racket[whoCanJoin]: Permissions to join the group. Possible values are: ANYONE_CAN_JOIN ALL_IN_DOMAIN_CAN_JOIN INVITED_CAN_JOIN CAN_REQUEST_TO_JOIN

@racket[whoCanPostMessage]: Permissions to post messages to the group. Possible values are: NONE_CAN_POST ALL_MANAGERS_CAN_POST ALL_MEMBERS_CAN_POST ALL_IN_DOMAIN_CAN_POST ANYONE_CAN_POST

@racket[whoCanViewGroup]: Permissions to view group. Possbile values are: ANYONE_CAN_VIEW ALL_IN_DOMAIN_CAN_VIEW ALL_MEMBERS_CAN_VIEW ALL_MANAGERS_CAN_VIEW

@racket[whoCanViewMembership]: Permissions to view membership. Possbile values are: ALL_IN_DOMAIN_CAN_VIEW ALL_MEMBERS_CAN_VIEW ALL_MANAGERS_CAN_VIEW

}

@defproc[(groupsSettings.groups.update
[groupUniqueId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:email email string? 'N/A]
[#:allowExternalMembers allowExternalMembers string? 'N/A]
[#:allowGoogleCommunication allowGoogleCommunication string? 'N/A]
[#:allowWebPosting allowWebPosting string? 'N/A]
[#:archiveOnly archiveOnly string? 'N/A]
[#:customReplyTo customReplyTo string? 'N/A]
[#:defaultMessageDenyNotificationText defaultMessageDenyNotificationText string? 'N/A]
[#:includeInGlobalAddressList includeInGlobalAddressList string? 'N/A]
[#:isArchived isArchived string? 'N/A]
[#:maxMessageBytes maxMessageBytes string? 'N/A]
[#:membersCanPostAsTheGroup membersCanPostAsTheGroup string? 'N/A]
[#:messageDisplayFont messageDisplayFont string? 'N/A]
[#:messageModerationLevel messageModerationLevel string? 'N/A]
[#:primaryLanguage primaryLanguage string? 'N/A]
[#:replyTo replyTo string? 'N/A]
[#:sendMessageDenyNotification sendMessageDenyNotification string? 'N/A]
[#:showInGroupDirectory showInGroupDirectory string? 'N/A]
[#:spamModerationLevel spamModerationLevel string? 'N/A]
[#:whoCanInvite whoCanInvite string? 'N/A]
[#:whoCanJoin whoCanJoin string? 'N/A]
[#:whoCanPostMessage whoCanPostMessage string? 'N/A]
[#:whoCanViewGroup whoCanViewGroup string? 'N/A]
[#:whoCanViewMembership whoCanViewMembership string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing resource.

@racket[groupUniqueId]: The resource ID

@racket[kind]: The type of the resource.

@racket[name]: Name of the Group

@racket[description]: Description of the group

@racket[email]: Email id of the group

@racket[allowExternalMembers]: Are external members allowed to join the group.

@racket[allowGoogleCommunication]: Is google allowed to contact admins.

@racket[allowWebPosting]: If posting from web is allowed.

@racket[archiveOnly]: If the group is archive only

@racket[customReplyTo]: Default email to which reply to any message should go.

@racket[defaultMessageDenyNotificationText]: Default message deny notification message

@racket[includeInGlobalAddressList]: If this groups should be included in global address list or not.

@racket[isArchived]: If the contents of the group are archived.

@racket[maxMessageBytes]: Maximum message size allowed.

@racket[membersCanPostAsTheGroup]: Can members post using the group email address.

@racket[messageDisplayFont]: Default message display font. Possible values are: DEFAULT_FONT FIXED_WIDTH_FONT

@racket[messageModerationLevel]: Moderation level for messages. Possible values are: MODERATE_ALL_MESSAGES MODERATE_NON_MEMBERS MODERATE_NEW_MEMBERS MODERATE_NONE

@racket[primaryLanguage]: Primary language for the group.

@racket[replyTo]: Whome should the default reply to a message go to. Possible values are: REPLY_TO_CUSTOM REPLY_TO_SENDER REPLY_TO_LIST REPLY_TO_OWNER REPLY_TO_IGNORE REPLY_TO_MANAGERS

@racket[sendMessageDenyNotification]: Should the member be notified if his message is denied by owner.

@racket[showInGroupDirectory]: Is the group listed in groups directory

@racket[spamModerationLevel]: Moderation level for messages detected as spam. Possible values are: ALLOW MODERATE SILENTLY_MODERATE REJECT

@racket[whoCanInvite]: Permissions to invite members. Possbile values are: ALL_MEMBERS_CAN_INVITE ALL_MANAGERS_CAN_INVITE

@racket[whoCanJoin]: Permissions to join the group. Possible values are: ANYONE_CAN_JOIN ALL_IN_DOMAIN_CAN_JOIN INVITED_CAN_JOIN CAN_REQUEST_TO_JOIN

@racket[whoCanPostMessage]: Permissions to post messages to the group. Possible values are: NONE_CAN_POST ALL_MANAGERS_CAN_POST ALL_MEMBERS_CAN_POST ALL_IN_DOMAIN_CAN_POST ANYONE_CAN_POST

@racket[whoCanViewGroup]: Permissions to view group. Possbile values are: ANYONE_CAN_VIEW ALL_IN_DOMAIN_CAN_VIEW ALL_MEMBERS_CAN_VIEW ALL_MANAGERS_CAN_VIEW

@racket[whoCanViewMembership]: Permissions to view membership. Possbile values are: ALL_IN_DOMAIN_CAN_VIEW ALL_MEMBERS_CAN_VIEW ALL_MANAGERS_CAN_VIEW

}

