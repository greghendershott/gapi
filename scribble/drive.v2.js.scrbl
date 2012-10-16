#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Drive API v2}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
The API to interact with Drive.
@hyperlink["https://developers.google.com/drive/" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "drive.v2.js")]
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

@subsection{replies}
@defproc[(drive-replies-list
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists all of the replies to a comment.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

@racket[maxResults]: The maximum number of replies to include in the response, used for paging.

@racket[pageToken]: The continuation token, used to page through large result sets. To get the next page of results, set this parameter to the value of "nextPageToken" from the previous response.

}

@defproc[(drive-replies-get
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:replyId replyId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a reply.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

@racket[replyId]: The ID of the reply.

}

@defproc[(drive-replies-insert
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:content content string? 'N/A]
[#:verb verb string? 'N/A]
[#:deleted deleted string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:htmlContent htmlContent string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:replyId replyId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new reply to the given comment.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

@racket[kind]: This is always drive#commentReply.

@racket[author]: The user who wrote this reply.

@racket[content]: The plain text content used to create this reply. This is not HTML safe and should only be used as a starting point to make edits to a reply's content. This field is required on inserts if no verb is specified (resolve/reopen).

@racket[verb]: The action this reply performed to the parent comment. When creating a new reply this is the action to be perform to the parent comment. Possible values are:  
- "resolve" - To resolve a comment. 
- "reopen" - To reopen (un-resolve) a comment.

@racket[deleted]: Whether this reply has been deleted. If a reply has been deleted the content will be cleared and this will only represent a reply that once existed.

@racket[createdDate]: The date when this reply was first created.

@racket[htmlContent]: HTML formatted content for this reply.

@racket[modifiedDate]: The date when this reply was last modified.

@racket[replyId]: The ID of the reply.

}

@defproc[(drive-replies-patch
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:replyId replyId string?]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:content content string? 'N/A]
[#:verb verb string? 'N/A]
[#:deleted deleted string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:htmlContent htmlContent string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing reply. This method supports patch semantics.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

@racket[replyId]: The ID of the reply.

@racket[kind]: This is always drive#commentReply.

@racket[author]: The user who wrote this reply.

@racket[content]: The plain text content used to create this reply. This is not HTML safe and should only be used as a starting point to make edits to a reply's content. This field is required on inserts if no verb is specified (resolve/reopen).

@racket[verb]: The action this reply performed to the parent comment. When creating a new reply this is the action to be perform to the parent comment. Possible values are:  
- "resolve" - To resolve a comment. 
- "reopen" - To reopen (un-resolve) a comment.

@racket[deleted]: Whether this reply has been deleted. If a reply has been deleted the content will be cleared and this will only represent a reply that once existed.

@racket[createdDate]: The date when this reply was first created.

@racket[htmlContent]: HTML formatted content for this reply.

@racket[modifiedDate]: The date when this reply was last modified.

}

@defproc[(drive-replies-update
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:replyId replyId string?]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:content content string? 'N/A]
[#:verb verb string? 'N/A]
[#:deleted deleted string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:htmlContent htmlContent string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing reply.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

@racket[replyId]: The ID of the reply.

@racket[kind]: This is always drive#commentReply.

@racket[author]: The user who wrote this reply.

@racket[content]: The plain text content used to create this reply. This is not HTML safe and should only be used as a starting point to make edits to a reply's content. This field is required on inserts if no verb is specified (resolve/reopen).

@racket[verb]: The action this reply performed to the parent comment. When creating a new reply this is the action to be perform to the parent comment. Possible values are:  
- "resolve" - To resolve a comment. 
- "reopen" - To reopen (un-resolve) a comment.

@racket[deleted]: Whether this reply has been deleted. If a reply has been deleted the content will be cleared and this will only represent a reply that once existed.

@racket[createdDate]: The date when this reply was first created.

@racket[htmlContent]: HTML formatted content for this reply.

@racket[modifiedDate]: The date when this reply was last modified.

}

@defproc[(drive-replies-delete
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:replyId replyId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes a reply.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

@racket[replyId]: The ID of the reply.

}

@subsection{comments}
@defproc[(drive-comments-list
[#:fileId fileId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:includeDeleted includeDeleted string? 'N/A]
[#:updatedMin updatedMin string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists a file's comments.

@racket[fileId]: The ID of the file.

@racket[maxResults]: The maximum number of discussions to include in the response, used for paging.

@racket[pageToken]: The continuation token, used to page through large result sets. To get the next page of results, set this parameter to the value of "nextPageToken" from the previous response.

@racket[includeDeleted]: If set, all comments, including deleted comments (with content stripped) will be returned.

@racket[updatedMin]: Only discussions that were updated after this timestamp will be returned. Formatted as an RFC 3339 timestamp.

}

@defproc[(drive-comments-get
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a comment by ID.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

}

@defproc[(drive-comments-insert
[#:fileId fileId string?]
[#:anchor anchor string? 'N/A]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:content content string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:replies replies string? 'N/A]
[#:commentId commentId string? 'N/A]
[#:deleted deleted string? 'N/A]
[#:status status string? 'N/A]
[#:context context string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:fileTitle fileTitle string? 'N/A]
[#:htmlContent htmlContent string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new comment on the given file.

@racket[fileId]: The ID of the file.

@racket[anchor]: A region of the document represented as a JSON string. See anchor documentation for details on how to define and interpret anchor properties.

@racket[kind]: This is always drive#comment.

@racket[author]: The user who wrote this comment.

@racket[content]: The plain text content used to create this comment. This is not HTML safe and should only be used as a starting point to make edits to a comment's content.

@racket[selfLink]: A link back to this comment.

@racket[replies]: Replies to this post.

@racket[commentId]: The ID of the comment.

@racket[deleted]: Whether this comment has been deleted. If a comment has been deleted the content will be cleared and this will only represent a comment that once existed.

@racket[status]: The status of this comment. Status can be changed by posting a reply to a comment with the desired status.  
- "open" - The comment is still open. 
- "resolved" - The comment has been resolved by one of its replies.

@racket[context]: The context of the file which is being commented on.

@racket[createdDate]: The date when this comment was first created.

@racket[fileTitle]: The title of the file which this comment is addressing.

@racket[htmlContent]: HTML formatted content for this comment.

@racket[modifiedDate]: The date when this comment or any of its replies were last modified.

}

@defproc[(drive-comments-patch
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:anchor anchor string? 'N/A]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:content content string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:replies replies string? 'N/A]
[#:deleted deleted string? 'N/A]
[#:status status string? 'N/A]
[#:context context string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:fileTitle fileTitle string? 'N/A]
[#:htmlContent htmlContent string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing comment. This method supports patch semantics.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

@racket[anchor]: A region of the document represented as a JSON string. See anchor documentation for details on how to define and interpret anchor properties.

@racket[kind]: This is always drive#comment.

@racket[author]: The user who wrote this comment.

@racket[content]: The plain text content used to create this comment. This is not HTML safe and should only be used as a starting point to make edits to a comment's content.

@racket[selfLink]: A link back to this comment.

@racket[replies]: Replies to this post.

@racket[deleted]: Whether this comment has been deleted. If a comment has been deleted the content will be cleared and this will only represent a comment that once existed.

@racket[status]: The status of this comment. Status can be changed by posting a reply to a comment with the desired status.  
- "open" - The comment is still open. 
- "resolved" - The comment has been resolved by one of its replies.

@racket[context]: The context of the file which is being commented on.

@racket[createdDate]: The date when this comment was first created.

@racket[fileTitle]: The title of the file which this comment is addressing.

@racket[htmlContent]: HTML formatted content for this comment.

@racket[modifiedDate]: The date when this comment or any of its replies were last modified.

}

@defproc[(drive-comments-update
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:anchor anchor string? 'N/A]
[#:kind kind string? 'N/A]
[#:author author string? 'N/A]
[#:content content string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:replies replies string? 'N/A]
[#:deleted deleted string? 'N/A]
[#:status status string? 'N/A]
[#:context context string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:fileTitle fileTitle string? 'N/A]
[#:htmlContent htmlContent string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing comment.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

@racket[anchor]: A region of the document represented as a JSON string. See anchor documentation for details on how to define and interpret anchor properties.

@racket[kind]: This is always drive#comment.

@racket[author]: The user who wrote this comment.

@racket[content]: The plain text content used to create this comment. This is not HTML safe and should only be used as a starting point to make edits to a comment's content.

@racket[selfLink]: A link back to this comment.

@racket[replies]: Replies to this post.

@racket[deleted]: Whether this comment has been deleted. If a comment has been deleted the content will be cleared and this will only represent a comment that once existed.

@racket[status]: The status of this comment. Status can be changed by posting a reply to a comment with the desired status.  
- "open" - The comment is still open. 
- "resolved" - The comment has been resolved by one of its replies.

@racket[context]: The context of the file which is being commented on.

@racket[createdDate]: The date when this comment was first created.

@racket[fileTitle]: The title of the file which this comment is addressing.

@racket[htmlContent]: HTML formatted content for this comment.

@racket[modifiedDate]: The date when this comment or any of its replies were last modified.

}

@defproc[(drive-comments-delete
[#:commentId commentId string?]
[#:fileId fileId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes a comment.

@racket[commentId]: The ID of the comment.

@racket[fileId]: The ID of the file.

}

@subsection{files}
@defproc[(drive-files-list
[#:projection projection string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:q q string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists the user's files.

@racket[projection]: This parameter is deprecated and has no function.

@racket[maxResults]: Maximum number of files to return.

@racket[pageToken]: Page token for files.

@racket[q]: Query string for searching files.

}

@defproc[(drive-files-touch
[#:fileId fileId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Set the file's updated time to the current server time.

@racket[fileId]: The ID of the file to update.

}

@defproc[(drive-files-get
[#:fileId fileId string?]
[#:projection projection string? 'N/A]
[#:updateViewedDate updateViewedDate string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a file's metadata by ID.

@racket[fileId]: The ID for the file in question.

@racket[projection]: This parameter is deprecated and has no function.

@racket[updateViewedDate]: Whether to update the view date after successfully retrieving the file.

}

@defproc[(drive-files-insert
[#:convert convert string? 'N/A]
[#:pinned pinned string? 'N/A]
[#:ocr ocr string? 'N/A]
[#:ocrLanguage ocrLanguage string? 'N/A]
[#:sourceLanguage sourceLanguage string? 'N/A]
[#:targetLanguage targetLanguage string? 'N/A]
[#:timedTextLanguage timedTextLanguage string? 'N/A]
[#:timedTextTrackName timedTextTrackName string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:labels labels string? 'N/A]
[#:quotaBytesUsed quotaBytesUsed string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:mimeType mimeType string? 'N/A]
[#:alternateLink alternateLink string? 'N/A]
[#:downloadUrl downloadUrl string? 'N/A]
[#:editable editable string? 'N/A]
[#:embedLink embedLink string? 'N/A]
[#:explicitlyTrashed explicitlyTrashed string? 'N/A]
[#:exportLinks exportLinks string? 'N/A]
[#:fileExtension fileExtension string? 'N/A]
[#:fileSize fileSize string? 'N/A]
[#:imageMediaMetadata imageMediaMetadata string? 'N/A]
[#:indexableText indexableText string? 'N/A]
[#:lastModifyingUserName lastModifyingUserName string? 'N/A]
[#:lastViewedByMeDate lastViewedByMeDate string? 'N/A]
[#:md5Checksum md5Checksum string? 'N/A]
[#:modifiedByMeDate modifiedByMeDate string? 'N/A]
[#:originalFilename originalFilename string? 'N/A]
[#:ownerNames ownerNames string? 'N/A]
[#:parents parents string? 'N/A]
[#:sharedWithMeDate sharedWithMeDate string? 'N/A]
[#:thumbnail thumbnail string? 'N/A]
[#:thumbnailLink thumbnailLink string? 'N/A]
[#:userPermission userPermission string? 'N/A]
[#:webContentLink webContentLink string? 'N/A]
[#:writersCanShare writersCanShare string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Insert a new file.

@racket[convert]: Whether to convert this file to the corresponding Google Docs format.

@racket[pinned]: Whether to pin the head revision of the uploaded file.

@racket[ocr]: Whether to attempt OCR on .jpg, .png, .gif, or .pdf uploads.

@racket[ocrLanguage]: If ocr is true, hints at the language to use. Valid values are ISO 639-1 codes.

@racket[sourceLanguage]: The language of the original file to be translated.

@racket[targetLanguage]: Target language to translate the file to. If no sourceLanguage is provided, the API will attempt to detect the language.

@racket[timedTextLanguage]: The language of the timed text.

@racket[timedTextTrackName]: The timed text track name.

@racket[id]: The id of the file.

@racket[kind]: The type of file. This is always drive#file.

@racket[title]: The title of this file.

@racket[description]: A short description of the file.

@racket[selfLink]: A link back to this file.

@racket[etag]: ETag of the file.

@racket[labels]: A group of labels for the file.

@racket[quotaBytesUsed]: The number of quota bytes used by this file.

@racket[createdDate]: Create time for this file (formatted ISO8601 timestamp).

@racket[modifiedDate]: Last time this file was modified by anyone (formatted RFC 3339 timestamp). This is only mutable on update when the setModifiedDate parameter is set.

@racket[mimeType]: The MIME type of the file. This is only mutable on update when uploading new content. This field can be left blank, and the mimetype will be determined from the uploaded content's MIME type.

@racket[alternateLink]: A link for opening the file in using a relevant Google editor or viewer.

@racket[downloadUrl]: Short term download URL for the file. This will only be populated on files with content stored in Drive.

@racket[editable]: Whether the file can be edited by the current user.

@racket[embedLink]: A link for embedding the file.

@racket[explicitlyTrashed]: Whether this file has been explicitly trashed, as opposed to recursively trashed. This will only be populated if the file is trashed.

@racket[exportLinks]: Links for exporting Google Docs to specific formats.

@racket[fileExtension]: The file extension used when downloading this file. This field is set from the title when inserting or uploading new content. This will only be populated on files with content stored in Drive.

@racket[fileSize]: The size of the file in bytes. This will only be populated on files with content stored in Drive.

@racket[imageMediaMetadata]: Metadata about image media. This will only be present for image types, and its contents will depend on what can be parsed from the image content.

@racket[indexableText]: Indexable text attributes for the file (can only be written)

@racket[lastModifyingUserName]: Name of the last user to modify this file. This will only be populated if a user has edited this file.

@racket[lastViewedByMeDate]: Last time this file was viewed by the user (formatted RFC 3339 timestamp).

@racket[md5Checksum]: An MD5 checksum for the content of this file. This will only be populated on files with content stored in Drive.

@racket[modifiedByMeDate]: Last time this file was modified by the user (formatted RFC 3339 timestamp). Note that setting modifiedDate will also update the modifiedByMe date for the user which set the date.

@racket[originalFilename]: The original filename if the file was uploaded manually, or the original title if the file was inserted through the API. Note that renames of the title will not change the original filename. This will only be populated on files with content stored in Drive.

@racket[ownerNames]: Name(s) of the owner(s) of this file.

@racket[parents]: Collection of parent folders which contain this file.
Setting this field will put the file in all of the provided folders. On insert, if no folders are provided, the file will be placed in the default root folder.

@racket[sharedWithMeDate]: Time at which this file was shared with the user (formatted RFC 3339 timestamp).

@racket[thumbnail]: Thumbnail for the file. Only accepted on upload and for files that are not already thumbnailed by Google.

@racket[thumbnailLink]: A link to the file's thumbnail.

@racket[userPermission]: The permissions for the authenticated user on this file.

@racket[webContentLink]: A link for downloading the content of the file in a browser using cookie based authentication. In cases where the content is shared publicly, the content can be downloaded without any credentials.

@racket[writersCanShare]: Whether writers can share the document with other users.

}

@defproc[(drive-files-patch
[#:fileId fileId string?]
[#:convert convert string? 'N/A]
[#:pinned pinned string? 'N/A]
[#:ocr ocr string? 'N/A]
[#:ocrLanguage ocrLanguage string? 'N/A]
[#:sourceLanguage sourceLanguage string? 'N/A]
[#:targetLanguage targetLanguage string? 'N/A]
[#:timedTextLanguage timedTextLanguage string? 'N/A]
[#:timedTextTrackName timedTextTrackName string? 'N/A]
[#:updateViewedDate updateViewedDate string? 'N/A]
[#:newRevision newRevision string? 'N/A]
[#:setModifiedDate setModifiedDate string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:labels labels string? 'N/A]
[#:quotaBytesUsed quotaBytesUsed string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:mimeType mimeType string? 'N/A]
[#:alternateLink alternateLink string? 'N/A]
[#:downloadUrl downloadUrl string? 'N/A]
[#:editable editable string? 'N/A]
[#:embedLink embedLink string? 'N/A]
[#:explicitlyTrashed explicitlyTrashed string? 'N/A]
[#:exportLinks exportLinks string? 'N/A]
[#:fileExtension fileExtension string? 'N/A]
[#:fileSize fileSize string? 'N/A]
[#:imageMediaMetadata imageMediaMetadata string? 'N/A]
[#:indexableText indexableText string? 'N/A]
[#:lastModifyingUserName lastModifyingUserName string? 'N/A]
[#:lastViewedByMeDate lastViewedByMeDate string? 'N/A]
[#:md5Checksum md5Checksum string? 'N/A]
[#:modifiedByMeDate modifiedByMeDate string? 'N/A]
[#:originalFilename originalFilename string? 'N/A]
[#:ownerNames ownerNames string? 'N/A]
[#:parents parents string? 'N/A]
[#:sharedWithMeDate sharedWithMeDate string? 'N/A]
[#:thumbnail thumbnail string? 'N/A]
[#:thumbnailLink thumbnailLink string? 'N/A]
[#:userPermission userPermission string? 'N/A]
[#:webContentLink webContentLink string? 'N/A]
[#:writersCanShare writersCanShare string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates file metadata and/or content. This method supports patch semantics.

@racket[fileId]: The ID of the file to update.

@racket[convert]: Whether to convert this file to the corresponding Google Docs format.

@racket[pinned]: Whether to pin the new revision.

@racket[ocr]: Whether to attempt OCR on .jpg, .png, .gif, or .pdf uploads.

@racket[ocrLanguage]: If ocr is true, hints at the language to use. Valid values are ISO 639-1 codes.

@racket[sourceLanguage]: The language of the original file to be translated.

@racket[targetLanguage]: Target language to translate the file to. If no sourceLanguage is provided, the API will attempt to detect the language.

@racket[timedTextLanguage]: The language of the timed text.

@racket[timedTextTrackName]: The timed text track name.

@racket[updateViewedDate]: Whether to update the view date after successfully updating the file.

@racket[newRevision]: Whether a blob upload should create a new revision. If false, the blob data in the current head revision will be replaced.

@racket[setModifiedDate]: Whether to set the modified date with the supplied modified date.

@racket[id]: The id of the file.

@racket[kind]: The type of file. This is always drive#file.

@racket[title]: The title of this file.

@racket[description]: A short description of the file.

@racket[selfLink]: A link back to this file.

@racket[etag]: ETag of the file.

@racket[labels]: A group of labels for the file.

@racket[quotaBytesUsed]: The number of quota bytes used by this file.

@racket[createdDate]: Create time for this file (formatted ISO8601 timestamp).

@racket[modifiedDate]: Last time this file was modified by anyone (formatted RFC 3339 timestamp). This is only mutable on update when the setModifiedDate parameter is set.

@racket[mimeType]: The MIME type of the file. This is only mutable on update when uploading new content. This field can be left blank, and the mimetype will be determined from the uploaded content's MIME type.

@racket[alternateLink]: A link for opening the file in using a relevant Google editor or viewer.

@racket[downloadUrl]: Short term download URL for the file. This will only be populated on files with content stored in Drive.

@racket[editable]: Whether the file can be edited by the current user.

@racket[embedLink]: A link for embedding the file.

@racket[explicitlyTrashed]: Whether this file has been explicitly trashed, as opposed to recursively trashed. This will only be populated if the file is trashed.

@racket[exportLinks]: Links for exporting Google Docs to specific formats.

@racket[fileExtension]: The file extension used when downloading this file. This field is set from the title when inserting or uploading new content. This will only be populated on files with content stored in Drive.

@racket[fileSize]: The size of the file in bytes. This will only be populated on files with content stored in Drive.

@racket[imageMediaMetadata]: Metadata about image media. This will only be present for image types, and its contents will depend on what can be parsed from the image content.

@racket[indexableText]: Indexable text attributes for the file (can only be written)

@racket[lastModifyingUserName]: Name of the last user to modify this file. This will only be populated if a user has edited this file.

@racket[lastViewedByMeDate]: Last time this file was viewed by the user (formatted RFC 3339 timestamp).

@racket[md5Checksum]: An MD5 checksum for the content of this file. This will only be populated on files with content stored in Drive.

@racket[modifiedByMeDate]: Last time this file was modified by the user (formatted RFC 3339 timestamp). Note that setting modifiedDate will also update the modifiedByMe date for the user which set the date.

@racket[originalFilename]: The original filename if the file was uploaded manually, or the original title if the file was inserted through the API. Note that renames of the title will not change the original filename. This will only be populated on files with content stored in Drive.

@racket[ownerNames]: Name(s) of the owner(s) of this file.

@racket[parents]: Collection of parent folders which contain this file.
Setting this field will put the file in all of the provided folders. On insert, if no folders are provided, the file will be placed in the default root folder.

@racket[sharedWithMeDate]: Time at which this file was shared with the user (formatted RFC 3339 timestamp).

@racket[thumbnail]: Thumbnail for the file. Only accepted on upload and for files that are not already thumbnailed by Google.

@racket[thumbnailLink]: A link to the file's thumbnail.

@racket[userPermission]: The permissions for the authenticated user on this file.

@racket[webContentLink]: A link for downloading the content of the file in a browser using cookie based authentication. In cases where the content is shared publicly, the content can be downloaded without any credentials.

@racket[writersCanShare]: Whether writers can share the document with other users.

}

@defproc[(drive-files-copy
[#:fileId fileId string?]
[#:convert convert string? 'N/A]
[#:pinned pinned string? 'N/A]
[#:ocr ocr string? 'N/A]
[#:ocrLanguage ocrLanguage string? 'N/A]
[#:sourceLanguage sourceLanguage string? 'N/A]
[#:targetLanguage targetLanguage string? 'N/A]
[#:timedTextLanguage timedTextLanguage string? 'N/A]
[#:timedTextTrackName timedTextTrackName string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:labels labels string? 'N/A]
[#:quotaBytesUsed quotaBytesUsed string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:mimeType mimeType string? 'N/A]
[#:alternateLink alternateLink string? 'N/A]
[#:downloadUrl downloadUrl string? 'N/A]
[#:editable editable string? 'N/A]
[#:embedLink embedLink string? 'N/A]
[#:explicitlyTrashed explicitlyTrashed string? 'N/A]
[#:exportLinks exportLinks string? 'N/A]
[#:fileExtension fileExtension string? 'N/A]
[#:fileSize fileSize string? 'N/A]
[#:imageMediaMetadata imageMediaMetadata string? 'N/A]
[#:indexableText indexableText string? 'N/A]
[#:lastModifyingUserName lastModifyingUserName string? 'N/A]
[#:lastViewedByMeDate lastViewedByMeDate string? 'N/A]
[#:md5Checksum md5Checksum string? 'N/A]
[#:modifiedByMeDate modifiedByMeDate string? 'N/A]
[#:originalFilename originalFilename string? 'N/A]
[#:ownerNames ownerNames string? 'N/A]
[#:parents parents string? 'N/A]
[#:sharedWithMeDate sharedWithMeDate string? 'N/A]
[#:thumbnail thumbnail string? 'N/A]
[#:thumbnailLink thumbnailLink string? 'N/A]
[#:userPermission userPermission string? 'N/A]
[#:webContentLink webContentLink string? 'N/A]
[#:writersCanShare writersCanShare string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a copy of the specified file.

@racket[fileId]: The ID of the file to copy.

@racket[convert]: Whether to convert this file to the corresponding Google Docs format.

@racket[pinned]: Whether to pin the head revision of the new copy.

@racket[ocr]: Whether to attempt OCR on .jpg, .png, .gif, or .pdf uploads.

@racket[ocrLanguage]: If ocr is true, hints at the language to use. Valid values are ISO 639-1 codes.

@racket[sourceLanguage]: The language of the original file to be translated.

@racket[targetLanguage]: Target language to translate the file to. If no sourceLanguage is provided, the API will attempt to detect the language.

@racket[timedTextLanguage]: The language of the timed text.

@racket[timedTextTrackName]: The timed text track name.

@racket[id]: The id of the file.

@racket[kind]: The type of file. This is always drive#file.

@racket[title]: The title of this file.

@racket[description]: A short description of the file.

@racket[selfLink]: A link back to this file.

@racket[etag]: ETag of the file.

@racket[labels]: A group of labels for the file.

@racket[quotaBytesUsed]: The number of quota bytes used by this file.

@racket[createdDate]: Create time for this file (formatted ISO8601 timestamp).

@racket[modifiedDate]: Last time this file was modified by anyone (formatted RFC 3339 timestamp). This is only mutable on update when the setModifiedDate parameter is set.

@racket[mimeType]: The MIME type of the file. This is only mutable on update when uploading new content. This field can be left blank, and the mimetype will be determined from the uploaded content's MIME type.

@racket[alternateLink]: A link for opening the file in using a relevant Google editor or viewer.

@racket[downloadUrl]: Short term download URL for the file. This will only be populated on files with content stored in Drive.

@racket[editable]: Whether the file can be edited by the current user.

@racket[embedLink]: A link for embedding the file.

@racket[explicitlyTrashed]: Whether this file has been explicitly trashed, as opposed to recursively trashed. This will only be populated if the file is trashed.

@racket[exportLinks]: Links for exporting Google Docs to specific formats.

@racket[fileExtension]: The file extension used when downloading this file. This field is set from the title when inserting or uploading new content. This will only be populated on files with content stored in Drive.

@racket[fileSize]: The size of the file in bytes. This will only be populated on files with content stored in Drive.

@racket[imageMediaMetadata]: Metadata about image media. This will only be present for image types, and its contents will depend on what can be parsed from the image content.

@racket[indexableText]: Indexable text attributes for the file (can only be written)

@racket[lastModifyingUserName]: Name of the last user to modify this file. This will only be populated if a user has edited this file.

@racket[lastViewedByMeDate]: Last time this file was viewed by the user (formatted RFC 3339 timestamp).

@racket[md5Checksum]: An MD5 checksum for the content of this file. This will only be populated on files with content stored in Drive.

@racket[modifiedByMeDate]: Last time this file was modified by the user (formatted RFC 3339 timestamp). Note that setting modifiedDate will also update the modifiedByMe date for the user which set the date.

@racket[originalFilename]: The original filename if the file was uploaded manually, or the original title if the file was inserted through the API. Note that renames of the title will not change the original filename. This will only be populated on files with content stored in Drive.

@racket[ownerNames]: Name(s) of the owner(s) of this file.

@racket[parents]: Collection of parent folders which contain this file.
Setting this field will put the file in all of the provided folders. On insert, if no folders are provided, the file will be placed in the default root folder.

@racket[sharedWithMeDate]: Time at which this file was shared with the user (formatted RFC 3339 timestamp).

@racket[thumbnail]: Thumbnail for the file. Only accepted on upload and for files that are not already thumbnailed by Google.

@racket[thumbnailLink]: A link to the file's thumbnail.

@racket[userPermission]: The permissions for the authenticated user on this file.

@racket[webContentLink]: A link for downloading the content of the file in a browser using cookie based authentication. In cases where the content is shared publicly, the content can be downloaded without any credentials.

@racket[writersCanShare]: Whether writers can share the document with other users.

}

@defproc[(drive-files-trash
[#:fileId fileId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Moves a file to the trash.

@racket[fileId]: The ID of the file to trash.

}

@defproc[(drive-files-untrash
[#:fileId fileId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Restores a file from the trash.

@racket[fileId]: The ID of the file to untrash.

}

@defproc[(drive-files-update
[#:fileId fileId string?]
[#:convert convert string? 'N/A]
[#:pinned pinned string? 'N/A]
[#:ocr ocr string? 'N/A]
[#:ocrLanguage ocrLanguage string? 'N/A]
[#:sourceLanguage sourceLanguage string? 'N/A]
[#:targetLanguage targetLanguage string? 'N/A]
[#:timedTextLanguage timedTextLanguage string? 'N/A]
[#:timedTextTrackName timedTextTrackName string? 'N/A]
[#:updateViewedDate updateViewedDate string? 'N/A]
[#:newRevision newRevision string? 'N/A]
[#:setModifiedDate setModifiedDate string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:labels labels string? 'N/A]
[#:quotaBytesUsed quotaBytesUsed string? 'N/A]
[#:createdDate createdDate string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:mimeType mimeType string? 'N/A]
[#:alternateLink alternateLink string? 'N/A]
[#:downloadUrl downloadUrl string? 'N/A]
[#:editable editable string? 'N/A]
[#:embedLink embedLink string? 'N/A]
[#:explicitlyTrashed explicitlyTrashed string? 'N/A]
[#:exportLinks exportLinks string? 'N/A]
[#:fileExtension fileExtension string? 'N/A]
[#:fileSize fileSize string? 'N/A]
[#:imageMediaMetadata imageMediaMetadata string? 'N/A]
[#:indexableText indexableText string? 'N/A]
[#:lastModifyingUserName lastModifyingUserName string? 'N/A]
[#:lastViewedByMeDate lastViewedByMeDate string? 'N/A]
[#:md5Checksum md5Checksum string? 'N/A]
[#:modifiedByMeDate modifiedByMeDate string? 'N/A]
[#:originalFilename originalFilename string? 'N/A]
[#:ownerNames ownerNames string? 'N/A]
[#:parents parents string? 'N/A]
[#:sharedWithMeDate sharedWithMeDate string? 'N/A]
[#:thumbnail thumbnail string? 'N/A]
[#:thumbnailLink thumbnailLink string? 'N/A]
[#:userPermission userPermission string? 'N/A]
[#:webContentLink webContentLink string? 'N/A]
[#:writersCanShare writersCanShare string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates file metadata and/or content

@racket[fileId]: The ID of the file to update.

@racket[convert]: Whether to convert this file to the corresponding Google Docs format.

@racket[pinned]: Whether to pin the new revision.

@racket[ocr]: Whether to attempt OCR on .jpg, .png, .gif, or .pdf uploads.

@racket[ocrLanguage]: If ocr is true, hints at the language to use. Valid values are ISO 639-1 codes.

@racket[sourceLanguage]: The language of the original file to be translated.

@racket[targetLanguage]: Target language to translate the file to. If no sourceLanguage is provided, the API will attempt to detect the language.

@racket[timedTextLanguage]: The language of the timed text.

@racket[timedTextTrackName]: The timed text track name.

@racket[updateViewedDate]: Whether to update the view date after successfully updating the file.

@racket[newRevision]: Whether a blob upload should create a new revision. If false, the blob data in the current head revision will be replaced.

@racket[setModifiedDate]: Whether to set the modified date with the supplied modified date.

@racket[id]: The id of the file.

@racket[kind]: The type of file. This is always drive#file.

@racket[title]: The title of this file.

@racket[description]: A short description of the file.

@racket[selfLink]: A link back to this file.

@racket[etag]: ETag of the file.

@racket[labels]: A group of labels for the file.

@racket[quotaBytesUsed]: The number of quota bytes used by this file.

@racket[createdDate]: Create time for this file (formatted ISO8601 timestamp).

@racket[modifiedDate]: Last time this file was modified by anyone (formatted RFC 3339 timestamp). This is only mutable on update when the setModifiedDate parameter is set.

@racket[mimeType]: The MIME type of the file. This is only mutable on update when uploading new content. This field can be left blank, and the mimetype will be determined from the uploaded content's MIME type.

@racket[alternateLink]: A link for opening the file in using a relevant Google editor or viewer.

@racket[downloadUrl]: Short term download URL for the file. This will only be populated on files with content stored in Drive.

@racket[editable]: Whether the file can be edited by the current user.

@racket[embedLink]: A link for embedding the file.

@racket[explicitlyTrashed]: Whether this file has been explicitly trashed, as opposed to recursively trashed. This will only be populated if the file is trashed.

@racket[exportLinks]: Links for exporting Google Docs to specific formats.

@racket[fileExtension]: The file extension used when downloading this file. This field is set from the title when inserting or uploading new content. This will only be populated on files with content stored in Drive.

@racket[fileSize]: The size of the file in bytes. This will only be populated on files with content stored in Drive.

@racket[imageMediaMetadata]: Metadata about image media. This will only be present for image types, and its contents will depend on what can be parsed from the image content.

@racket[indexableText]: Indexable text attributes for the file (can only be written)

@racket[lastModifyingUserName]: Name of the last user to modify this file. This will only be populated if a user has edited this file.

@racket[lastViewedByMeDate]: Last time this file was viewed by the user (formatted RFC 3339 timestamp).

@racket[md5Checksum]: An MD5 checksum for the content of this file. This will only be populated on files with content stored in Drive.

@racket[modifiedByMeDate]: Last time this file was modified by the user (formatted RFC 3339 timestamp). Note that setting modifiedDate will also update the modifiedByMe date for the user which set the date.

@racket[originalFilename]: The original filename if the file was uploaded manually, or the original title if the file was inserted through the API. Note that renames of the title will not change the original filename. This will only be populated on files with content stored in Drive.

@racket[ownerNames]: Name(s) of the owner(s) of this file.

@racket[parents]: Collection of parent folders which contain this file.
Setting this field will put the file in all of the provided folders. On insert, if no folders are provided, the file will be placed in the default root folder.

@racket[sharedWithMeDate]: Time at which this file was shared with the user (formatted RFC 3339 timestamp).

@racket[thumbnail]: Thumbnail for the file. Only accepted on upload and for files that are not already thumbnailed by Google.

@racket[thumbnailLink]: A link to the file's thumbnail.

@racket[userPermission]: The permissions for the authenticated user on this file.

@racket[webContentLink]: A link for downloading the content of the file in a browser using cookie based authentication. In cases where the content is shared publicly, the content can be downloaded without any credentials.

@racket[writersCanShare]: Whether writers can share the document with other users.

}

@defproc[(drive-files-delete
[#:fileId fileId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Permanently deletes a file by ID. Skips the trash.

@racket[fileId]: The ID of the file to delete.

}

@subsection{parents}
@defproc[(drive-parents-list
[#:fileId fileId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists a file's parents.

@racket[fileId]: The ID of the file.

}

@defproc[(drive-parents-get
[#:fileId fileId string?]
[#:parentId parentId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a specific parent reference.

@racket[fileId]: The ID of the file.

@racket[parentId]: The ID of the parent.

}

@defproc[(drive-parents-insert
[#:fileId fileId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:isRoot isRoot string? 'N/A]
[#:parentLink parentLink string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Adds a parent folder for a file.

@racket[fileId]: The ID of the file.

@racket[id]: The ID of the parent.

@racket[kind]: This is always drive#parentReference.

@racket[selfLink]: A link back to this reference.

@racket[isRoot]: Whether or not the parent is the root folder.

@racket[parentLink]: A link to the parent.

}

@defproc[(drive-parents-delete
[#:fileId fileId string?]
[#:parentId parentId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Removes a parent from a file.

@racket[fileId]: The ID of the file.

@racket[parentId]: The ID of the parent.

}

@subsection{about}
@defproc[(drive-about-get
[#:includeSubscribed includeSubscribed string? 'N/A]
[#:maxChangeIdCount maxChangeIdCount string? 'N/A]
[#:startChangeId startChangeId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets the information about the current user along with Drive API settings

@racket[includeSubscribed]: Whether to include subscribed items when calculating the number of remaining change IDs

@racket[maxChangeIdCount]: Maximum number of remaining change IDs to count

@racket[startChangeId]: Change ID to start counting from when calculating number of remaining change IDs

}

@subsection{apps}
@defproc[(drive-apps-list
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists a user's apps.

}

@defproc[(drive-apps-get
[#:appId appId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a specific app.

@racket[appId]: The ID of the app.

}

@subsection{changes}
@defproc[(drive-changes-list
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:includeSubscribed includeSubscribed string? 'N/A]
[#:startChangeId startChangeId string? 'N/A]
[#:includeDeleted includeDeleted string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists the changes for a user.

@racket[maxResults]: Maximum number of changes to return.

@racket[pageToken]: Page token for changes.

@racket[includeSubscribed]: Whether to include subscribed items.

@racket[startChangeId]: Change ID to start listing changes from.

@racket[includeDeleted]: Whether to include deleted items.

}

@defproc[(drive-changes-get
[#:changeId changeId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a specific change.

@racket[changeId]: The ID of the change.

}

@subsection{children}
@defproc[(drive-children-list
[#:folderId folderId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:q q string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists a folder's children.

@racket[folderId]: The ID of the folder.

@racket[maxResults]: Maximum number of children to return.

@racket[pageToken]: Page token for children.

@racket[q]: Query string for searching children.

}

@defproc[(drive-children-get
[#:childId childId string?]
[#:folderId folderId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a specific child reference.

@racket[childId]: The ID of the child.

@racket[folderId]: The ID of the folder.

}

@defproc[(drive-children-insert
[#:folderId folderId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:childLink childLink string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a file into a folder.

@racket[folderId]: The ID of the folder.

@racket[id]: The ID of the child.

@racket[kind]: This is always drive#childReference.

@racket[selfLink]: A link back to this reference.

@racket[childLink]: A link to the child.

}

@defproc[(drive-children-delete
[#:childId childId string?]
[#:folderId folderId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Removes a child from a folder.

@racket[childId]: The ID of the child.

@racket[folderId]: The ID of the folder.

}

@subsection{permissions}
@defproc[(drive-permissions-list
[#:fileId fileId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists a file's permissions.

@racket[fileId]: The ID for the file.

}

@defproc[(drive-permissions-get
[#:fileId fileId string?]
[#:permissionId permissionId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a permission by ID.

@racket[fileId]: The ID for the file.

@racket[permissionId]: The ID for the permission.

}

@defproc[(drive-permissions-insert
[#:fileId fileId string?]
[#:sendNotificationEmails sendNotificationEmails string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:value value string? 'N/A]
[#:role role string? 'N/A]
[#:additionalRoles additionalRoles string? 'N/A]
[#:authKey authKey string? 'N/A]
[#:photoLink photoLink string? 'N/A]
[#:withLink withLink string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Inserts a permission for a file.

@racket[fileId]: The ID for the file.

@racket[sendNotificationEmails]: Whether to send notification emails.

@racket[id]: The ID of the permission.

@racket[kind]: This is always drive#permission.

@racket[name]: The name for this permission.

@racket[type]: The account type. Allowed values are:  
- user 
- group 
- domain 
- anyone

@racket[selfLink]: A link back to this permission.

@racket[etag]: The ETag of the permission.

@racket[value]: The email address or domain name for the entity. This is not populated in responses.

@racket[role]: The primary role for this user. Allowed values are:  
- owner 
- reader 
- writer

@racket[additionalRoles]: Additional roles for this user. Only commenter is currently allowed.

@racket[authKey]: The authkey parameter required for this permission.

@racket[photoLink]: A link to the profile photo, if available.

@racket[withLink]: Whether the link is required for this permission.

}

@defproc[(drive-permissions-patch
[#:fileId fileId string?]
[#:permissionId permissionId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:value value string? 'N/A]
[#:role role string? 'N/A]
[#:additionalRoles additionalRoles string? 'N/A]
[#:authKey authKey string? 'N/A]
[#:photoLink photoLink string? 'N/A]
[#:withLink withLink string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a permission. This method supports patch semantics.

@racket[fileId]: The ID for the file.

@racket[permissionId]: The ID for the permission.

@racket[id]: The ID of the permission.

@racket[kind]: This is always drive#permission.

@racket[name]: The name for this permission.

@racket[type]: The account type. Allowed values are:  
- user 
- group 
- domain 
- anyone

@racket[selfLink]: A link back to this permission.

@racket[etag]: The ETag of the permission.

@racket[value]: The email address or domain name for the entity. This is not populated in responses.

@racket[role]: The primary role for this user. Allowed values are:  
- owner 
- reader 
- writer

@racket[additionalRoles]: Additional roles for this user. Only commenter is currently allowed.

@racket[authKey]: The authkey parameter required for this permission.

@racket[photoLink]: A link to the profile photo, if available.

@racket[withLink]: Whether the link is required for this permission.

}

@defproc[(drive-permissions-update
[#:fileId fileId string?]
[#:permissionId permissionId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:value value string? 'N/A]
[#:role role string? 'N/A]
[#:additionalRoles additionalRoles string? 'N/A]
[#:authKey authKey string? 'N/A]
[#:photoLink photoLink string? 'N/A]
[#:withLink withLink string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a permission.

@racket[fileId]: The ID for the file.

@racket[permissionId]: The ID for the permission.

@racket[id]: The ID of the permission.

@racket[kind]: This is always drive#permission.

@racket[name]: The name for this permission.

@racket[type]: The account type. Allowed values are:  
- user 
- group 
- domain 
- anyone

@racket[selfLink]: A link back to this permission.

@racket[etag]: The ETag of the permission.

@racket[value]: The email address or domain name for the entity. This is not populated in responses.

@racket[role]: The primary role for this user. Allowed values are:  
- owner 
- reader 
- writer

@racket[additionalRoles]: Additional roles for this user. Only commenter is currently allowed.

@racket[authKey]: The authkey parameter required for this permission.

@racket[photoLink]: A link to the profile photo, if available.

@racket[withLink]: Whether the link is required for this permission.

}

@defproc[(drive-permissions-delete
[#:fileId fileId string?]
[#:permissionId permissionId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes a permission from a file.

@racket[fileId]: The ID for the file.

@racket[permissionId]: The ID for the permission.

}

@subsection{revisions}
@defproc[(drive-revisions-list
[#:fileId fileId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists a file's revisions.

@racket[fileId]: The ID of the file.

}

@defproc[(drive-revisions-get
[#:fileId fileId string?]
[#:revisionId revisionId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a specific revision.

@racket[fileId]: The ID of the file.

@racket[revisionId]: The ID of the revision.

}

@defproc[(drive-revisions-patch
[#:fileId fileId string?]
[#:revisionId revisionId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:published published string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:mimeType mimeType string? 'N/A]
[#:downloadUrl downloadUrl string? 'N/A]
[#:exportLinks exportLinks string? 'N/A]
[#:fileSize fileSize string? 'N/A]
[#:lastModifyingUserName lastModifyingUserName string? 'N/A]
[#:md5Checksum md5Checksum string? 'N/A]
[#:originalFilename originalFilename string? 'N/A]
[#:pinned pinned string? 'N/A]
[#:publishAuto publishAuto string? 'N/A]
[#:publishedLink publishedLink string? 'N/A]
[#:publishedOutsideDomain publishedOutsideDomain string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a revision. This method supports patch semantics.

@racket[fileId]: The ID for the file.

@racket[revisionId]: The ID for the revision.

@racket[id]: The ID of the revision.

@racket[kind]: This is always drive#revision.

@racket[selfLink]: A link back to this revision.

@racket[etag]: The ETag of the revision.

@racket[published]: Whether this revision is published. This is only populated and can only be modified for Google Docs.

@racket[modifiedDate]: Last time this revision was modified (formatted RFC 3339 timestamp).

@racket[mimeType]: The MIME type of the revision.

@racket[downloadUrl]: Short term download URL for the file. This will only be populated on files with content stored in Drive.

@racket[exportLinks]: Links for exporting Google Docs to specific formats.

@racket[fileSize]: The size of the revision in bytes. This will only be populated on files with content stored in Drive.

@racket[lastModifyingUserName]: Name of the last user to modify this revision.

@racket[md5Checksum]: An MD5 checksum for the content of this revision. This will only be populated on files with content stored in Drive.

@racket[originalFilename]: The original filename when this revision was created. This will only be populated on files with content stored in Drive.

@racket[pinned]: Whether this revision is pinned to prevent automatic purging. This will only be populated and can only be modified on files with content stored in Drive which are not Google Docs. Revisions can also be pinned when they are created through the drive.files.insert/update/copy by using the pinned query parameter.

@racket[publishAuto]: Whether subsequent revisions will be automatically republished. This is only populated and can only be modified for Google Docs.

@racket[publishedLink]: A link to the published revision.

@racket[publishedOutsideDomain]: Whether this revision is published outside the domain. This is only populated and can only be modified for Google Docs.

}

@defproc[(drive-revisions-update
[#:fileId fileId string?]
[#:revisionId revisionId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:published published string? 'N/A]
[#:modifiedDate modifiedDate string? 'N/A]
[#:mimeType mimeType string? 'N/A]
[#:downloadUrl downloadUrl string? 'N/A]
[#:exportLinks exportLinks string? 'N/A]
[#:fileSize fileSize string? 'N/A]
[#:lastModifyingUserName lastModifyingUserName string? 'N/A]
[#:md5Checksum md5Checksum string? 'N/A]
[#:originalFilename originalFilename string? 'N/A]
[#:pinned pinned string? 'N/A]
[#:publishAuto publishAuto string? 'N/A]
[#:publishedLink publishedLink string? 'N/A]
[#:publishedOutsideDomain publishedOutsideDomain string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a revision.

@racket[fileId]: The ID for the file.

@racket[revisionId]: The ID for the revision.

@racket[id]: The ID of the revision.

@racket[kind]: This is always drive#revision.

@racket[selfLink]: A link back to this revision.

@racket[etag]: The ETag of the revision.

@racket[published]: Whether this revision is published. This is only populated and can only be modified for Google Docs.

@racket[modifiedDate]: Last time this revision was modified (formatted RFC 3339 timestamp).

@racket[mimeType]: The MIME type of the revision.

@racket[downloadUrl]: Short term download URL for the file. This will only be populated on files with content stored in Drive.

@racket[exportLinks]: Links for exporting Google Docs to specific formats.

@racket[fileSize]: The size of the revision in bytes. This will only be populated on files with content stored in Drive.

@racket[lastModifyingUserName]: Name of the last user to modify this revision.

@racket[md5Checksum]: An MD5 checksum for the content of this revision. This will only be populated on files with content stored in Drive.

@racket[originalFilename]: The original filename when this revision was created. This will only be populated on files with content stored in Drive.

@racket[pinned]: Whether this revision is pinned to prevent automatic purging. This will only be populated and can only be modified on files with content stored in Drive which are not Google Docs. Revisions can also be pinned when they are created through the drive.files.insert/update/copy by using the pinned query parameter.

@racket[publishAuto]: Whether subsequent revisions will be automatically republished. This is only populated and can only be modified for Google Docs.

@racket[publishedLink]: A link to the published revision.

@racket[publishedOutsideDomain]: Whether this revision is published outside the domain. This is only populated and can only be modified for Google Docs.

}

@defproc[(drive-revisions-delete
[#:fileId fileId string?]
[#:revisionId revisionId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Removes a revision.

@racket[fileId]: The ID of the file.

@racket[revisionId]: The ID of the revision.

}

