#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Tasks API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you manage your tasks and task lists.
@hyperlink["http://code.google.com/apis/tasks/v1/using.html" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "tasks.v1.js")]
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

@subsection{tasks}
@defproc[(tasks-tasks-list
[#:tasklist tasklist string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:completedMax completedMax string? 'N/A]
[#:completedMin completedMin string? 'N/A]
[#:dueMax dueMax string? 'N/A]
[#:dueMin dueMin string? 'N/A]
[#:showCompleted showCompleted string? 'N/A]
[#:showDeleted showDeleted string? 'N/A]
[#:showHidden showHidden string? 'N/A]
[#:updatedMin updatedMin string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns all tasks in the specified task list.

@racket[tasklist]: Task list identifier.

@racket[maxResults]: Maximum number of task lists returned on one page. Optional. The default is 100.

@racket[pageToken]: Token specifying the result page to return. Optional.

@racket[completedMax]: Upper bound for a task's completion date (as a RFC 3339 timestamp) to filter by. Optional. The default is not to filter by completion date.

@racket[completedMin]: Lower bound for a task's completion date (as a RFC 3339 timestamp) to filter by. Optional. The default is not to filter by completion date.

@racket[dueMax]: Upper bound for a task's due date (as a RFC 3339 timestamp) to filter by. Optional. The default is not to filter by due date.

@racket[dueMin]: Lower bound for a task's due date (as a RFC 3339 timestamp) to filter by. Optional. The default is not to filter by due date.

@racket[showCompleted]: Flag indicating whether completed tasks are returned in the result. Optional. The default is True.

@racket[showDeleted]: Flag indicating whether deleted tasks are returned in the result. Optional. The default is False.

@racket[showHidden]: Flag indicating whether hidden tasks are returned in the result. Optional. The default is False.

@racket[updatedMin]: Lower bound for a task's last modification time (as a RFC 3339 timestamp) to filter by. Optional. The default is not to filter by last modification time.

}

@defproc[(tasks-tasks-get
[#:task task string?]
[#:tasklist tasklist string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified task.

@racket[task]: Task identifier.

@racket[tasklist]: Task list identifier.

}

@defproc[(tasks-tasks-insert
[#:tasklist tasklist string?]
[#:parent parent string? 'N/A]
[#:previous previous string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:completed completed string? 'N/A]
[#:due due string? 'N/A]
[#:links links string? 'N/A]
[#:notes notes string? 'N/A]
[#:parent parent string? 'N/A]
[#:position position string? 'N/A]
[#:deleted deleted string? 'N/A]
[#:hidden hidden string? 'N/A]
[#:status status string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new task on the specified task list.

@racket[tasklist]: Task list identifier.

@racket[parent]: Parent task identifier. If the task is created at the top level, this parameter is omitted. Optional.

@racket[previous]: Previous sibling task identifier. If the task is created at the first position among its siblings, this parameter is omitted. Optional.

@racket[id]: Task identifier.

@racket[kind]: Type of the resource. This is always "tasks#task".

@racket[title]: Title of the task.

@racket[selfLink]: URL pointing to this task. Used to retrieve, update, or delete this task.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the task (as a RFC 3339 timestamp).

@racket[completed]: Completion date of the task (as a RFC 3339 timestamp). This field is omitted if the task has not been completed.

@racket[due]: Due date of the task (as a RFC 3339 timestamp). Optional.

@racket[links]: Collection of links. This collection is read-only.

@racket[notes]: Notes describing the task. Optional.

@racket[parent]: Parent task identifier. This field is omitted if it is a top-level task. This field is read-only. Use the "move" method to move the task under a different parent or to the top level.

@racket[position]: String indicating the position of the task among its sibling tasks under the same parent task or at the top level. If this string is greater than another task's corresponding position string according to lexicographical ordering, the task is positioned after the other task under the same parent task (or at the top level). This field is read-only. Use the "move" method to move the task to another position.

@racket[deleted]: Flag indicating whether the task has been deleted. The default if False.

@racket[hidden]: Flag indicating whether the task is hidden. This is the case if the task had been marked completed when the task list was last cleared. The default is False. This field is read-only.

@racket[status]: Status of the task. This is either "needsAction" or "completed".

}

@defproc[(tasks-tasks-clear
[#:tasklist tasklist string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Clears all completed tasks from the specified task list. The affected tasks will be marked as 'hidden' and no longer be returned by default when retrieving all tasks for a task list.

@racket[tasklist]: Task list identifier.

}

@defproc[(tasks-tasks-move
[#:task task string?]
[#:tasklist tasklist string?]
[#:parent parent string? 'N/A]
[#:previous previous string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Moves the specified task to another position in the task list. This can include putting it as a child task under a new parent and/or move it to a different position among its sibling tasks.

@racket[task]: Task identifier.

@racket[tasklist]: Task list identifier.

@racket[parent]: New parent task identifier. If the task is moved to the top level, this parameter is omitted. Optional.

@racket[previous]: New previous sibling task identifier. If the task is moved to the first position among its siblings, this parameter is omitted. Optional.

}

@defproc[(tasks-tasks-patch
[#:task task string?]
[#:tasklist tasklist string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:completed completed string? 'N/A]
[#:due due string? 'N/A]
[#:links links string? 'N/A]
[#:notes notes string? 'N/A]
[#:parent parent string? 'N/A]
[#:position position string? 'N/A]
[#:deleted deleted string? 'N/A]
[#:hidden hidden string? 'N/A]
[#:status status string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the specified task. This method supports patch semantics.

@racket[task]: Task identifier.

@racket[tasklist]: Task list identifier.

@racket[id]: Task identifier.

@racket[kind]: Type of the resource. This is always "tasks#task".

@racket[title]: Title of the task.

@racket[selfLink]: URL pointing to this task. Used to retrieve, update, or delete this task.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the task (as a RFC 3339 timestamp).

@racket[completed]: Completion date of the task (as a RFC 3339 timestamp). This field is omitted if the task has not been completed.

@racket[due]: Due date of the task (as a RFC 3339 timestamp). Optional.

@racket[links]: Collection of links. This collection is read-only.

@racket[notes]: Notes describing the task. Optional.

@racket[parent]: Parent task identifier. This field is omitted if it is a top-level task. This field is read-only. Use the "move" method to move the task under a different parent or to the top level.

@racket[position]: String indicating the position of the task among its sibling tasks under the same parent task or at the top level. If this string is greater than another task's corresponding position string according to lexicographical ordering, the task is positioned after the other task under the same parent task (or at the top level). This field is read-only. Use the "move" method to move the task to another position.

@racket[deleted]: Flag indicating whether the task has been deleted. The default if False.

@racket[hidden]: Flag indicating whether the task is hidden. This is the case if the task had been marked completed when the task list was last cleared. The default is False. This field is read-only.

@racket[status]: Status of the task. This is either "needsAction" or "completed".

}

@defproc[(tasks-tasks-update
[#:task task string?]
[#:tasklist tasklist string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:completed completed string? 'N/A]
[#:due due string? 'N/A]
[#:links links string? 'N/A]
[#:notes notes string? 'N/A]
[#:parent parent string? 'N/A]
[#:position position string? 'N/A]
[#:deleted deleted string? 'N/A]
[#:hidden hidden string? 'N/A]
[#:status status string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the specified task.

@racket[task]: Task identifier.

@racket[tasklist]: Task list identifier.

@racket[id]: Task identifier.

@racket[kind]: Type of the resource. This is always "tasks#task".

@racket[title]: Title of the task.

@racket[selfLink]: URL pointing to this task. Used to retrieve, update, or delete this task.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the task (as a RFC 3339 timestamp).

@racket[completed]: Completion date of the task (as a RFC 3339 timestamp). This field is omitted if the task has not been completed.

@racket[due]: Due date of the task (as a RFC 3339 timestamp). Optional.

@racket[links]: Collection of links. This collection is read-only.

@racket[notes]: Notes describing the task. Optional.

@racket[parent]: Parent task identifier. This field is omitted if it is a top-level task. This field is read-only. Use the "move" method to move the task under a different parent or to the top level.

@racket[position]: String indicating the position of the task among its sibling tasks under the same parent task or at the top level. If this string is greater than another task's corresponding position string according to lexicographical ordering, the task is positioned after the other task under the same parent task (or at the top level). This field is read-only. Use the "move" method to move the task to another position.

@racket[deleted]: Flag indicating whether the task has been deleted. The default if False.

@racket[hidden]: Flag indicating whether the task is hidden. This is the case if the task had been marked completed when the task list was last cleared. The default is False. This field is read-only.

@racket[status]: Status of the task. This is either "needsAction" or "completed".

}

@defproc[(tasks-tasks-delete
[#:task task string?]
[#:tasklist tasklist string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the specified task from the task list.

@racket[task]: Task identifier.

@racket[tasklist]: Task list identifier.

}

@subsection{tasklists}
@defproc[(tasks-tasklists-list
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
Returns all the authenticated user's task lists.

@racket[maxResults]: Maximum number of task lists returned on one page. Optional. The default is 100.

@racket[pageToken]: Token specifying the result page to return. Optional.

}

@defproc[(tasks-tasklists-get
[#:tasklist tasklist string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the authenticated user's specified task list.

@racket[tasklist]: Task list identifier.

}

@defproc[(tasks-tasklists-insert
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new task list and adds it to the authenticated user's task lists.

@racket[id]: Task list identifier.

@racket[kind]: Type of the resource. This is always "tasks#taskList".

@racket[title]: Title of the task list.

@racket[selfLink]: URL pointing to this task list. Used to retrieve, update, or delete this task list.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the task list (as a RFC 3339 timestamp).

}

@defproc[(tasks-tasklists-patch
[#:tasklist tasklist string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the authenticated user's specified task list. This method supports patch semantics.

@racket[tasklist]: Task list identifier.

@racket[id]: Task list identifier.

@racket[kind]: Type of the resource. This is always "tasks#taskList".

@racket[title]: Title of the task list.

@racket[selfLink]: URL pointing to this task list. Used to retrieve, update, or delete this task list.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the task list (as a RFC 3339 timestamp).

}

@defproc[(tasks-tasklists-update
[#:tasklist tasklist string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:title title string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:updated updated string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the authenticated user's specified task list.

@racket[tasklist]: Task list identifier.

@racket[id]: Task list identifier.

@racket[kind]: Type of the resource. This is always "tasks#taskList".

@racket[title]: Title of the task list.

@racket[selfLink]: URL pointing to this task list. Used to retrieve, update, or delete this task list.

@racket[etag]: ETag of the resource.

@racket[updated]: Last modification time of the task list (as a RFC 3339 timestamp).

}

@defproc[(tasks-tasklists-delete
[#:tasklist tasklist string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the authenticated user's specified task list.

@racket[tasklist]: Task list identifier.

}

