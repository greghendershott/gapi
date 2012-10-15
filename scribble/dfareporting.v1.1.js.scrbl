#lang scribble/manual
@title{DFA Reporting API v1.1}
Lets you create, run and download reports.
@hyperlink["https://developers.google.com/doubleclick-advertisers/reporting/" "Documentation link"]
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


@section{Functions for the `files' resource}
@defproc[(dfareporting.files.list
[profileId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:sortOrder sortOrder string? 'N/A]
[#:sortField sortField string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Lists files for a user profile.

@racket[profileId]: The DFA profile ID.

@racket[maxResults]: Maximum number of results to return.

@racket[pageToken]: The value of the nextToken from the previous result page.

@racket[sortOrder]: Order of sorted results, default is 'DESCENDING'.

@racket[sortField]: The field by which to sort the list.

}

@section{Functions for the `dimensionValues' resource}
@defproc[(dfareporting.dimensionValues.query
[profileId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:kind kind string? 'N/A]
[#:filters filters string? 'N/A]
[#:endDate endDate string? 'N/A]
[#:startDate startDate string? 'N/A]
[#:dimensionName dimensionName string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves list of report dimension values for a list of filters.

@racket[profileId]: The DFA user profile ID.

@racket[maxResults]: Maximum number of results to return.

@racket[pageToken]: The value of the nextToken from the previous result page.

@racket[kind]: The kind of request this is, in this case dfareporting#dimensionValueRequest.

@racket[filters]: The list of filters by which to filter values. The filters are ANDed.

@racket[endDate]: The end date of the date range for which to retrieve dimension values. A string of the format: "yyyy-MM-dd".

@racket[startDate]: The start date of the date range for which to retrieve dimension values. A string of the format: "yyyy-MM-dd".

@racket[dimensionName]: The name of the dimension for which values should be requested.

}

@section{Functions for the `reports' resource}
@defproc[(dfareporting.reports.list
[profileId string?]
[#:maxResults maxResults string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:sortOrder sortOrder string? 'N/A]
[#:sortField sortField string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves list of reports.

@racket[profileId]: The DFA user profile ID.

@racket[maxResults]: Maximum number of results to return.

@racket[pageToken]: The value of the nextToken from the previous result page.

@racket[sortOrder]: Order of sorted results, default is 'DESCENDING'.

@racket[sortField]: The field by which to sort the list.

}

@defproc[(dfareporting.reports.get
[profileId string?]
[reportId string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a report by its ID.

@racket[profileId]: The DFA user profile ID.

@racket[reportId]: The ID of the report.

}

@defproc[(dfareporting.reports.insert
[profileId string?]
[#:format format string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:accountId accountId string? 'N/A]
[#:etag etag string? 'N/A]
[#:fileName fileName string? 'N/A]
[#:lastModifiedTime lastModifiedTime string? 'N/A]
[#:activeGrpCriteria activeGrpCriteria string? 'N/A]
[#:criteria criteria string? 'N/A]
[#:crossDimensionReachCriteria crossDimensionReachCriteria string? 'N/A]
[#:delivery delivery string? 'N/A]
[#:floodlightCriteria floodlightCriteria string? 'N/A]
[#:ownerProfileId ownerProfileId string? 'N/A]
[#:pathToConversionCriteria pathToConversionCriteria string? 'N/A]
[#:reachCriteria reachCriteria string? 'N/A]
[#:schedule schedule string? 'N/A]
[#:subAccountId subAccountId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a report.

@racket[profileId]: The DFA user profile ID.

@racket[format]: The output format of the report, currently only "CSV" is supported. If not specified, default format is "CSV". Note that the actual format in the completed report file might differ if for instance the report's size exceeds the format's capabilities. "CSV" will then be the fallback format.

@racket[id]: The unique ID identifying this report resource.

@racket[kind]: The kind of resource this is, in this case dfareporting#report.

@racket[name]: The name of the report.

@racket[type]: The type of the report, one of:  
- STANDARD 
- REACH 
- ACTIVE_GRP 
- PATH_TO_CONVERSION 
- FLOODLIGHT 
- CROSS_DIMENSION_REACH

@racket[accountId]: The account ID to which this report belongs.

@racket[etag]: The eTag of this response for caching purposes.

@racket[fileName]: The file name used when generating report files for this report.

@racket[lastModifiedTime]: The timestamp (in milliseconds since epoch) of when this report was last modified.

@racket[activeGrpCriteria]: The report criteria for a report of type "ACTIVE_GRP".

@racket[criteria]: The report criteria for a report of type "STANDARD".

@racket[crossDimensionReachCriteria]: The report criteria for a report of type "CROSS_DIMENSION_REACH".

@racket[delivery]: The report's email delivery settings.

@racket[floodlightCriteria]: The report criteria for a report of type "FLOODLIGHT".

@racket[ownerProfileId]: The user profile id of the owner of this report.

@racket[pathToConversionCriteria]: The report criteria for a report of type "PATH_TO_CONVERSION".

@racket[reachCriteria]: The report criteria for a report of type "REACH".

@racket[schedule]: The report's schedule. Can only be set if the report's 'dateRange' is a relative date range and the relative date range is not "TODAY".

@racket[subAccountId]: The subbaccount ID to which this report belongs if applicable.

}

@defproc[(dfareporting.reports.patch
[profileId string?]
[reportId string?]
[#:format format string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:accountId accountId string? 'N/A]
[#:etag etag string? 'N/A]
[#:fileName fileName string? 'N/A]
[#:lastModifiedTime lastModifiedTime string? 'N/A]
[#:activeGrpCriteria activeGrpCriteria string? 'N/A]
[#:criteria criteria string? 'N/A]
[#:crossDimensionReachCriteria crossDimensionReachCriteria string? 'N/A]
[#:delivery delivery string? 'N/A]
[#:floodlightCriteria floodlightCriteria string? 'N/A]
[#:ownerProfileId ownerProfileId string? 'N/A]
[#:pathToConversionCriteria pathToConversionCriteria string? 'N/A]
[#:reachCriteria reachCriteria string? 'N/A]
[#:schedule schedule string? 'N/A]
[#:subAccountId subAccountId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a report. This method supports patch semantics.

@racket[profileId]: The DFA user profile ID.

@racket[reportId]: The ID of the report.

@racket[format]: The output format of the report, currently only "CSV" is supported. If not specified, default format is "CSV". Note that the actual format in the completed report file might differ if for instance the report's size exceeds the format's capabilities. "CSV" will then be the fallback format.

@racket[id]: The unique ID identifying this report resource.

@racket[kind]: The kind of resource this is, in this case dfareporting#report.

@racket[name]: The name of the report.

@racket[type]: The type of the report, one of:  
- STANDARD 
- REACH 
- ACTIVE_GRP 
- PATH_TO_CONVERSION 
- FLOODLIGHT 
- CROSS_DIMENSION_REACH

@racket[accountId]: The account ID to which this report belongs.

@racket[etag]: The eTag of this response for caching purposes.

@racket[fileName]: The file name used when generating report files for this report.

@racket[lastModifiedTime]: The timestamp (in milliseconds since epoch) of when this report was last modified.

@racket[activeGrpCriteria]: The report criteria for a report of type "ACTIVE_GRP".

@racket[criteria]: The report criteria for a report of type "STANDARD".

@racket[crossDimensionReachCriteria]: The report criteria for a report of type "CROSS_DIMENSION_REACH".

@racket[delivery]: The report's email delivery settings.

@racket[floodlightCriteria]: The report criteria for a report of type "FLOODLIGHT".

@racket[ownerProfileId]: The user profile id of the owner of this report.

@racket[pathToConversionCriteria]: The report criteria for a report of type "PATH_TO_CONVERSION".

@racket[reachCriteria]: The report criteria for a report of type "REACH".

@racket[schedule]: The report's schedule. Can only be set if the report's 'dateRange' is a relative date range and the relative date range is not "TODAY".

@racket[subAccountId]: The subbaccount ID to which this report belongs if applicable.

}

@defproc[(dfareporting.reports.run
[profileId string?]
[reportId string?]
[#:synchronous synchronous string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Runs a report.

@racket[profileId]: The DFA profile ID.

@racket[reportId]: The ID of the report.

@racket[synchronous]: If set and true, tries to run the report synchronously.

}

@defproc[(dfareporting.reports.update
[profileId string?]
[reportId string?]
[#:format format string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:accountId accountId string? 'N/A]
[#:etag etag string? 'N/A]
[#:fileName fileName string? 'N/A]
[#:lastModifiedTime lastModifiedTime string? 'N/A]
[#:activeGrpCriteria activeGrpCriteria string? 'N/A]
[#:criteria criteria string? 'N/A]
[#:crossDimensionReachCriteria crossDimensionReachCriteria string? 'N/A]
[#:delivery delivery string? 'N/A]
[#:floodlightCriteria floodlightCriteria string? 'N/A]
[#:ownerProfileId ownerProfileId string? 'N/A]
[#:pathToConversionCriteria pathToConversionCriteria string? 'N/A]
[#:reachCriteria reachCriteria string? 'N/A]
[#:schedule schedule string? 'N/A]
[#:subAccountId subAccountId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a report.

@racket[profileId]: The DFA user profile ID.

@racket[reportId]: The ID of the report.

@racket[format]: The output format of the report, currently only "CSV" is supported. If not specified, default format is "CSV". Note that the actual format in the completed report file might differ if for instance the report's size exceeds the format's capabilities. "CSV" will then be the fallback format.

@racket[id]: The unique ID identifying this report resource.

@racket[kind]: The kind of resource this is, in this case dfareporting#report.

@racket[name]: The name of the report.

@racket[type]: The type of the report, one of:  
- STANDARD 
- REACH 
- ACTIVE_GRP 
- PATH_TO_CONVERSION 
- FLOODLIGHT 
- CROSS_DIMENSION_REACH

@racket[accountId]: The account ID to which this report belongs.

@racket[etag]: The eTag of this response for caching purposes.

@racket[fileName]: The file name used when generating report files for this report.

@racket[lastModifiedTime]: The timestamp (in milliseconds since epoch) of when this report was last modified.

@racket[activeGrpCriteria]: The report criteria for a report of type "ACTIVE_GRP".

@racket[criteria]: The report criteria for a report of type "STANDARD".

@racket[crossDimensionReachCriteria]: The report criteria for a report of type "CROSS_DIMENSION_REACH".

@racket[delivery]: The report's email delivery settings.

@racket[floodlightCriteria]: The report criteria for a report of type "FLOODLIGHT".

@racket[ownerProfileId]: The user profile id of the owner of this report.

@racket[pathToConversionCriteria]: The report criteria for a report of type "PATH_TO_CONVERSION".

@racket[reachCriteria]: The report criteria for a report of type "REACH".

@racket[schedule]: The report's schedule. Can only be set if the report's 'dateRange' is a relative date range and the relative date range is not "TODAY".

@racket[subAccountId]: The subbaccount ID to which this report belongs if applicable.

}

@defproc[(dfareporting.reports.delete
[profileId string?]
[reportId string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes a report by its ID.

@racket[profileId]: The DFA user profile ID.

@racket[reportId]: The ID of the report.

}

@section{Functions for the `userProfiles' resource}
@defproc[(dfareporting.userProfiles.list
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves list of user profiles for a user.

}

@defproc[(dfareporting.userProfiles.get
[profileId string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets one user profile by ID.

@racket[profileId]: The user profile ID.

}

