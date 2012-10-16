#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{BigQuery API v2}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
A data platform for customers to create, manage, share and query data.
@hyperlink["https://developers.google.com/bigquery/docs/overview" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "bigquery.v2.js")]
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

@subsection{datasets}
@defproc[(bigquery-datasets-list
[#:projectId projectId string?]
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
Lists all the datasets in the specified project to which the caller has read access; however, a project owner can list (but not necessarily get) all datasets in his project.

@racket[projectId]: Project ID of the datasets to be listed

@racket[maxResults]: The maximum number of results to return

@racket[pageToken]: Page token, returned by a previous call, to request the next page of results

}

@defproc[(bigquery-datasets-get
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the dataset specified by datasetID.

@racket[datasetId]: Dataset ID of the requested dataset

@racket[projectId]: Project ID of the requested dataset

}

@defproc[(bigquery-datasets-insert
[#:projectId projectId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:access access string? 'N/A]
[#:etag etag string? 'N/A]
[#:creationTime creationTime string? 'N/A]
[#:datasetReference datasetReference string? 'N/A]
[#:friendlyName friendlyName string? 'N/A]
[#:lastModifiedTime lastModifiedTime string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new empty dataset.

@racket[projectId]: Project ID of the new dataset

@racket[id]: [Output-only] The fully-qualified unique name of this dataset in the format projectId:datasetId. The dataset name without the project name is given in the datasetId field. When creating a new dataset, leave this field blank, and instead specify the datasetId field.

@racket[kind]: [Output-only] The resource type.

@racket[description]: [Optional] A user-friendly string description for the dataset. This might be shown in BigQuery UI for browsing the dataset.

@racket[selfLink]: [Output-only] An URL that can be used to access this resource again. You can use this URL in Get or Update requests to this resource.

@racket[access]: [Optional] Describes users' rights on the dataset. You can assign the same role to multiple users, and assign multiple roles to the same user.
Default values assigned to a new dataset are as follows: OWNER - Project owners, dataset creator READ - Project readers WRITE - Project writers
See ACLs and Rights for a description of these rights. If you specify any of these roles when creating a dataset, the assigned roles will overwrite the defaults listed above.
To revoke rights to a dataset, call datasets.update() and omit the names of anyone whose rights you wish to revoke. However, every dataset must have at least one entity granted OWNER role.
Each access object can have only one of the following members: userByEmail, groupByEmail, domain, or allAuthenticatedUsers.

@racket[etag]: [Output-only] A hash of this resource.

@racket[creationTime]: [Output-only] The time when this dataset was created, in milliseconds since the epoch.

@racket[datasetReference]: [Required] Reference identifying dataset.

@racket[friendlyName]: [Optional] A descriptive name for this dataset, which might be shown in any BigQuery user interfaces for browsing the dataset. Use datasetId for making API calls.

@racket[lastModifiedTime]: [Output-only] The date when this dataset or any of its tables was last modified, in milliseconds since the epoch.

}

@defproc[(bigquery-datasets-patch
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:access access string? 'N/A]
[#:etag etag string? 'N/A]
[#:creationTime creationTime string? 'N/A]
[#:datasetReference datasetReference string? 'N/A]
[#:friendlyName friendlyName string? 'N/A]
[#:lastModifiedTime lastModifiedTime string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates information in an existing dataset, specified by datasetId. Properties not included in the submitted resource will not be changed. If you include the access property without any values assigned, the request will fail as you must specify at least one owner for a dataset. This method supports patch semantics.

@racket[datasetId]: Dataset ID of the dataset being updated

@racket[projectId]: Project ID of the dataset being updated

@racket[id]: [Output-only] The fully-qualified unique name of this dataset in the format projectId:datasetId. The dataset name without the project name is given in the datasetId field. When creating a new dataset, leave this field blank, and instead specify the datasetId field.

@racket[kind]: [Output-only] The resource type.

@racket[description]: [Optional] A user-friendly string description for the dataset. This might be shown in BigQuery UI for browsing the dataset.

@racket[selfLink]: [Output-only] An URL that can be used to access this resource again. You can use this URL in Get or Update requests to this resource.

@racket[access]: [Optional] Describes users' rights on the dataset. You can assign the same role to multiple users, and assign multiple roles to the same user.
Default values assigned to a new dataset are as follows: OWNER - Project owners, dataset creator READ - Project readers WRITE - Project writers
See ACLs and Rights for a description of these rights. If you specify any of these roles when creating a dataset, the assigned roles will overwrite the defaults listed above.
To revoke rights to a dataset, call datasets.update() and omit the names of anyone whose rights you wish to revoke. However, every dataset must have at least one entity granted OWNER role.
Each access object can have only one of the following members: userByEmail, groupByEmail, domain, or allAuthenticatedUsers.

@racket[etag]: [Output-only] A hash of this resource.

@racket[creationTime]: [Output-only] The time when this dataset was created, in milliseconds since the epoch.

@racket[datasetReference]: [Required] Reference identifying dataset.

@racket[friendlyName]: [Optional] A descriptive name for this dataset, which might be shown in any BigQuery user interfaces for browsing the dataset. Use datasetId for making API calls.

@racket[lastModifiedTime]: [Output-only] The date when this dataset or any of its tables was last modified, in milliseconds since the epoch.

}

@defproc[(bigquery-datasets-update
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:access access string? 'N/A]
[#:etag etag string? 'N/A]
[#:creationTime creationTime string? 'N/A]
[#:datasetReference datasetReference string? 'N/A]
[#:friendlyName friendlyName string? 'N/A]
[#:lastModifiedTime lastModifiedTime string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates information in an existing dataset, specified by datasetId. Properties not included in the submitted resource will not be changed. If you include the access property without any values assigned, the request will fail as you must specify at least one owner for a dataset.

@racket[datasetId]: Dataset ID of the dataset being updated

@racket[projectId]: Project ID of the dataset being updated

@racket[id]: [Output-only] The fully-qualified unique name of this dataset in the format projectId:datasetId. The dataset name without the project name is given in the datasetId field. When creating a new dataset, leave this field blank, and instead specify the datasetId field.

@racket[kind]: [Output-only] The resource type.

@racket[description]: [Optional] A user-friendly string description for the dataset. This might be shown in BigQuery UI for browsing the dataset.

@racket[selfLink]: [Output-only] An URL that can be used to access this resource again. You can use this URL in Get or Update requests to this resource.

@racket[access]: [Optional] Describes users' rights on the dataset. You can assign the same role to multiple users, and assign multiple roles to the same user.
Default values assigned to a new dataset are as follows: OWNER - Project owners, dataset creator READ - Project readers WRITE - Project writers
See ACLs and Rights for a description of these rights. If you specify any of these roles when creating a dataset, the assigned roles will overwrite the defaults listed above.
To revoke rights to a dataset, call datasets.update() and omit the names of anyone whose rights you wish to revoke. However, every dataset must have at least one entity granted OWNER role.
Each access object can have only one of the following members: userByEmail, groupByEmail, domain, or allAuthenticatedUsers.

@racket[etag]: [Output-only] A hash of this resource.

@racket[creationTime]: [Output-only] The time when this dataset was created, in milliseconds since the epoch.

@racket[datasetReference]: [Required] Reference identifying dataset.

@racket[friendlyName]: [Optional] A descriptive name for this dataset, which might be shown in any BigQuery user interfaces for browsing the dataset. Use datasetId for making API calls.

@racket[lastModifiedTime]: [Output-only] The date when this dataset or any of its tables was last modified, in milliseconds since the epoch.

}

@defproc[(bigquery-datasets-delete
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:deleteContents deleteContents string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the dataset specified by datasetId value. Before you can delete a dataset, you must delete all its tables, either manually or by specifying deleteContents. Immediately after deletion, you can create another dataset with the same name.

@racket[datasetId]: Dataset ID of dataset being deleted

@racket[projectId]: Project ID of the dataset being deleted

@racket[deleteContents]: If True, delete all the tables in the dataset. If False and the dataset contains tables, the request will fail. Default is False

}

@subsection{jobs}
@defproc[(bigquery-jobs-list
[#:projectId projectId string?]
[#:projection projection string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:allUsers allUsers string? 'N/A]
[#:stateFilter stateFilter string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists all the Jobs in the specified project that were started by the user.

@racket[projectId]: Project ID of the jobs to list

@racket[projection]: Restrict information returned to a set of selected fields

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Page token, returned by a previous call, to request the next page of results

@racket[allUsers]: Whether to display jobs owned by all users in the project. Default false

@racket[stateFilter]: Filter for job state

}

@defproc[(bigquery-jobs-get
[#:projectId projectId string?]
[#:jobId jobId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the specified job by ID.

@racket[projectId]: Project ID of the requested job

@racket[jobId]: Job ID of the requested job

}

@defproc[(bigquery-jobs-query
[#:projectId projectId string?]
[#:kind kind string? 'N/A]
[#:query query string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:defaultDataset defaultDataset string? 'N/A]
[#:dryRun dryRun string? 'N/A]
[#:timeoutMs timeoutMs string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Runs a BigQuery SQL query synchronously and returns query results if the query completes within a specified timeout.

@racket[projectId]: Project ID of the project billed for the query

@racket[kind]: The resource type of the request.

@racket[query]: [Required] A query string, following the BigQuery query syntax of the query to execute. Table names should be qualified by dataset name in the format projectId:datasetId.tableId unless you specify the defaultDataset value. If the table is in the same project as the job, you can omit the project ID. Example: SELECT f1 FROM myProjectId:myDatasetId.myTableId.

@racket[maxResults]: [Optional] The maximum number of results to return per page of results. If the response list exceeds the maximum response size for a single response, you will have to page through the results. Default is to return the maximum response size.

@racket[defaultDataset]: [Optional] Specifies the default datasetId and projectId to assume for any unqualified table names in the query. If not set, all table names in the query string must be fully-qualified in the format projectId:datasetId.tableid.

@racket[dryRun]: [Optional] If set, don't actually run the query. A valid query will return an empty response, while an invalid query will return the same error it would if it wasn't a dry run.

@racket[timeoutMs]: [Optional] How long to wait for the query to complete, in milliseconds, before returning. Default is to return immediately. If the timeout passes before the job completes, the request will fail with a TIMEOUT error.

}

@defproc[(bigquery-jobs-insert
[#:projectId projectId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:status status string? 'N/A]
[#:jobReference jobReference string? 'N/A]
[#:configuration configuration string? 'N/A]
[#:statistics statistics string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Starts a new asynchronous job.

@racket[projectId]: Project ID of the project that will be billed for the job

@racket[id]: [Output-only] Opaque ID field of the job

@racket[kind]: [Output-only] The type of the resource.

@racket[selfLink]: [Output-only] A URL that can be used to access this resource again.

@racket[etag]: [Output-only] A hash of this resource.

@racket[status]: [Output-only] The status of this job. Examine this value when polling an asynchronous job to see if the job is complete.

@racket[jobReference]: [Optional] Reference describing the unique-per-user name of the job.

@racket[configuration]: [Required] Describes the job configuration.

@racket[statistics]: [Output-only] Information about the job, including starting time and ending time of the job.

}

@defproc[(bigquery-jobs-getQueryResults
[#:projectId projectId string?]
[#:jobId jobId string?]
[#:maxResults maxResults string? 'N/A]
[#:startIndex startIndex string? 'N/A]
[#:timeoutMs timeoutMs string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the results of a query job.

@racket[projectId]: Project ID of the query job

@racket[jobId]: Job ID of the query job

@racket[maxResults]: Maximum number of results to read

@racket[startIndex]: Zero-based index of the starting row

@racket[timeoutMs]: How long to wait for the query to complete, in milliseconds, before returning. Default is to return immediately. If the timeout passes before the job completes, the request will fail with a TIMEOUT error

}

@subsection{projects}
@defproc[(bigquery-projects-list
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
Lists the projects to which you have at least read access.

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Page token, returned by a previous call, to request the next page of results

}

@subsection{tables}
@defproc[(bigquery-tables-list
[#:datasetId datasetId string?]
[#:projectId projectId string?]
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
Lists all tables in the specified dataset.

@racket[datasetId]: Dataset ID of the tables to list

@racket[projectId]: Project ID of the tables to list

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Page token, returned by a previous call, to request the next page of results

}

@defproc[(bigquery-tables-get
[#:tableId tableId string?]
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets the specified table resource by table ID. This method does not return the data in the table, it only returns the table resource, which describes the structure of this table.

@racket[tableId]: Table ID of the requested table

@racket[datasetId]: Dataset ID of the requested table

@racket[projectId]: Project ID of the requested table

}

@defproc[(bigquery-tables-insert
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:schema schema string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:creationTime creationTime string? 'N/A]
[#:friendlyName friendlyName string? 'N/A]
[#:lastModifiedTime lastModifiedTime string? 'N/A]
[#:expirationTime expirationTime string? 'N/A]
[#:numBytes numBytes string? 'N/A]
[#:numRows numRows string? 'N/A]
[#:tableReference tableReference string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new, empty table in the dataset.

@racket[datasetId]: Dataset ID of the new table

@racket[projectId]: Project ID of the new table

@racket[id]: [Output-only] An opaque ID uniquely identifying the table.

@racket[kind]: [Output-only] The type of the resource.

@racket[schema]: [Optional] Describes the schema of this table.

@racket[description]: [Optional] A user-friendly description of this table.

@racket[selfLink]: [Output-only] A URL that can be used to access this resource again.

@racket[etag]: [Output-only] A hash of this resource.

@racket[creationTime]: [Output-only] The time when this table was created, in milliseconds since the epoch.

@racket[friendlyName]: [Optional] A descriptive name for this table.

@racket[lastModifiedTime]: [Output-only] The time when this table was last modified, in milliseconds since the epoch.

@racket[expirationTime]: [Optional] The time when this table expires, in milliseconds since the epoch. If not present, the table will persist indefinitely. Expired tables will be deleted and their storage reclaimed.

@racket[numBytes]: [Output-only] The size of the table in bytes.

@racket[numRows]: [Output-only] The number of rows of data in this table.

@racket[tableReference]: [Required] Reference describing the ID of this table.

}

@defproc[(bigquery-tables-patch
[#:tableId tableId string?]
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:schema schema string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:creationTime creationTime string? 'N/A]
[#:friendlyName friendlyName string? 'N/A]
[#:lastModifiedTime lastModifiedTime string? 'N/A]
[#:expirationTime expirationTime string? 'N/A]
[#:numBytes numBytes string? 'N/A]
[#:numRows numRows string? 'N/A]
[#:tableReference tableReference string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates information in an existing table, specified by tableId. This method supports patch semantics.

@racket[tableId]: Table ID of the table to update

@racket[datasetId]: Dataset ID of the table to update

@racket[projectId]: Project ID of the table to update

@racket[id]: [Output-only] An opaque ID uniquely identifying the table.

@racket[kind]: [Output-only] The type of the resource.

@racket[schema]: [Optional] Describes the schema of this table.

@racket[description]: [Optional] A user-friendly description of this table.

@racket[selfLink]: [Output-only] A URL that can be used to access this resource again.

@racket[etag]: [Output-only] A hash of this resource.

@racket[creationTime]: [Output-only] The time when this table was created, in milliseconds since the epoch.

@racket[friendlyName]: [Optional] A descriptive name for this table.

@racket[lastModifiedTime]: [Output-only] The time when this table was last modified, in milliseconds since the epoch.

@racket[expirationTime]: [Optional] The time when this table expires, in milliseconds since the epoch. If not present, the table will persist indefinitely. Expired tables will be deleted and their storage reclaimed.

@racket[numBytes]: [Output-only] The size of the table in bytes.

@racket[numRows]: [Output-only] The number of rows of data in this table.

@racket[tableReference]: [Required] Reference describing the ID of this table.

}

@defproc[(bigquery-tables-update
[#:tableId tableId string?]
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:schema schema string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:etag etag string? 'N/A]
[#:creationTime creationTime string? 'N/A]
[#:friendlyName friendlyName string? 'N/A]
[#:lastModifiedTime lastModifiedTime string? 'N/A]
[#:expirationTime expirationTime string? 'N/A]
[#:numBytes numBytes string? 'N/A]
[#:numRows numRows string? 'N/A]
[#:tableReference tableReference string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates information in an existing table, specified by tableId.

@racket[tableId]: Table ID of the table to update

@racket[datasetId]: Dataset ID of the table to update

@racket[projectId]: Project ID of the table to update

@racket[id]: [Output-only] An opaque ID uniquely identifying the table.

@racket[kind]: [Output-only] The type of the resource.

@racket[schema]: [Optional] Describes the schema of this table.

@racket[description]: [Optional] A user-friendly description of this table.

@racket[selfLink]: [Output-only] A URL that can be used to access this resource again.

@racket[etag]: [Output-only] A hash of this resource.

@racket[creationTime]: [Output-only] The time when this table was created, in milliseconds since the epoch.

@racket[friendlyName]: [Optional] A descriptive name for this table.

@racket[lastModifiedTime]: [Output-only] The time when this table was last modified, in milliseconds since the epoch.

@racket[expirationTime]: [Optional] The time when this table expires, in milliseconds since the epoch. If not present, the table will persist indefinitely. Expired tables will be deleted and their storage reclaimed.

@racket[numBytes]: [Output-only] The size of the table in bytes.

@racket[numRows]: [Output-only] The number of rows of data in this table.

@racket[tableReference]: [Required] Reference describing the ID of this table.

}

@defproc[(bigquery-tables-delete
[#:tableId tableId string?]
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the table specified by tableId from the dataset. If the table contains data, all the data will be deleted.

@racket[tableId]: Table ID of the table to delete

@racket[datasetId]: Dataset ID of the table to delete

@racket[projectId]: Project ID of the table to delete

}

@subsection{tabledata}
@defproc[(bigquery-tabledata-list
[#:tableId tableId string?]
[#:datasetId datasetId string?]
[#:projectId projectId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:startIndex startIndex string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves table data from a specified set of rows.

@racket[tableId]: Table ID of the table to read

@racket[datasetId]: Dataset ID of the table to read

@racket[projectId]: Project ID of the table to read

@racket[maxResults]: Maximum number of results to return

@racket[pageToken]: Page token, returned by a previous call, identifying the result set

@racket[startIndex]: Zero-based index of the starting row to read

}

