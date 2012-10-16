#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{TaskQueue API v1beta2}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
Lets you access a Google App Engine Pull Task Queue over REST.
@hyperlink["http://code.google.com/appengine/docs/python/taskqueue/rest.html" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "taskqueue.v1beta2.js")]
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

@subsection{taskqueues}
@defproc[(taskqueue-taskqueues-get
[project string?]
[taskqueue string?]
[#:getStats getStats string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get detailed information about a TaskQueue.

@racket[project]: The project under which the queue lies.

@racket[taskqueue]: The id of the taskqueue to get the properties of.

@racket[getStats]: Whether to get stats. Optional.

}

@subsection{tasks}
@defproc[(taskqueue-tasks-list
[project string?]
[taskqueue string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
List Tasks in a TaskQueue

@racket[project]: The project under which the queue lies.

@racket[taskqueue]: The id of the taskqueue to list tasks from.

}

@defproc[(taskqueue-tasks-get
[project string?]
[taskqueue string?]
[task string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Get a particular task from a TaskQueue.

@racket[project]: The project under which the queue lies.

@racket[taskqueue]: The taskqueue in which the task belongs.

@racket[task]: The task to get properties of.

}

@defproc[(taskqueue-tasks-insert
[project string?]
[taskqueue string?]
[#:id id string? 'N/A]
[#:tag tag string? 'N/A]
[#:kind kind string? 'N/A]
[#:enqueueTimestamp enqueueTimestamp string? 'N/A]
[#:leaseTimestamp leaseTimestamp string? 'N/A]
[#:payloadBase64 payloadBase64 string? 'N/A]
[#:queueName queueName string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Insert a new task in a TaskQueue

@racket[project]: The project under which the queue lies

@racket[taskqueue]: The taskqueue to insert the task into

@racket[id]: Name of the task.

@racket[tag]: Tag for the task, could be used later to lease tasks grouped by a specific tag.

@racket[kind]: The kind of object returned, in this case set to task.

@racket[enqueueTimestamp]: Time (in seconds since the epoch) at which the task was enqueued.

@racket[leaseTimestamp]: Time (in seconds since the epoch) at which the task lease will expire. This value is 0 if the task isnt currently leased out to a worker.

@racket[payloadBase64]: A bag of bytes which is the task payload. The payload on the JSON side is always Base64 encoded.

@racket[queueName]: Name of the queue that the task is in.

}

@defproc[(taskqueue-tasks-patch
[project string?]
[taskqueue string?]
[task string?]
[newLeaseSeconds string?]
[#:id id string? 'N/A]
[#:tag tag string? 'N/A]
[#:kind kind string? 'N/A]
[#:enqueueTimestamp enqueueTimestamp string? 'N/A]
[#:leaseTimestamp leaseTimestamp string? 'N/A]
[#:payloadBase64 payloadBase64 string? 'N/A]
[#:queueName queueName string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update tasks that are leased out of a TaskQueue. This method supports patch semantics.

@racket[project]: The project under which the queue lies.

@racket[taskqueue]: 

@racket[task]: 

@racket[newLeaseSeconds]: The new lease in seconds.

@racket[id]: Name of the task.

@racket[tag]: Tag for the task, could be used later to lease tasks grouped by a specific tag.

@racket[kind]: The kind of object returned, in this case set to task.

@racket[enqueueTimestamp]: Time (in seconds since the epoch) at which the task was enqueued.

@racket[leaseTimestamp]: Time (in seconds since the epoch) at which the task lease will expire. This value is 0 if the task isnt currently leased out to a worker.

@racket[payloadBase64]: A bag of bytes which is the task payload. The payload on the JSON side is always Base64 encoded.

@racket[queueName]: Name of the queue that the task is in.

}

@defproc[(taskqueue-tasks-lease
[project string?]
[taskqueue string?]
[leaseSecs string?]
[numTasks string?]
[#:tag tag string? 'N/A]
[#:groupByTag groupByTag string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lease 1 or more tasks from a TaskQueue.

@racket[project]: The project under which the queue lies.

@racket[taskqueue]: The taskqueue to lease a task from.

@racket[leaseSecs]: The lease in seconds.

@racket[numTasks]: The number of tasks to lease.

@racket[tag]: The tag allowed for tasks in the response. Must only be specified if group_by_tag is true. If group_by_tag is true and tag is not specified the tag will be that of the oldest task by eta, i.e. the first available tag

@racket[groupByTag]: When true, all returned tasks will have the same tag

}

@defproc[(taskqueue-tasks-update
[project string?]
[taskqueue string?]
[task string?]
[newLeaseSeconds string?]
[#:id id string? 'N/A]
[#:tag tag string? 'N/A]
[#:kind kind string? 'N/A]
[#:enqueueTimestamp enqueueTimestamp string? 'N/A]
[#:leaseTimestamp leaseTimestamp string? 'N/A]
[#:payloadBase64 payloadBase64 string? 'N/A]
[#:queueName queueName string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Update tasks that are leased out of a TaskQueue.

@racket[project]: The project under which the queue lies.

@racket[taskqueue]: 

@racket[task]: 

@racket[newLeaseSeconds]: The new lease in seconds.

@racket[id]: Name of the task.

@racket[tag]: Tag for the task, could be used later to lease tasks grouped by a specific tag.

@racket[kind]: The kind of object returned, in this case set to task.

@racket[enqueueTimestamp]: Time (in seconds since the epoch) at which the task was enqueued.

@racket[leaseTimestamp]: Time (in seconds since the epoch) at which the task lease will expire. This value is 0 if the task isnt currently leased out to a worker.

@racket[payloadBase64]: A bag of bytes which is the task payload. The payload on the JSON side is always Base64 encoded.

@racket[queueName]: Name of the queue that the task is in.

}

@defproc[(taskqueue-tasks-delete
[project string?]
[taskqueue string?]
[task string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Delete a task from a TaskQueue.

@racket[project]: The project under which the queue lies.

@racket[taskqueue]: The taskqueue to delete a task from.

@racket[task]: The id of the task to delete.

}

