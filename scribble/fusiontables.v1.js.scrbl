#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Fusion Tables API v1}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
API for working with Fusion Tables data.
@hyperlink["https://developers.google.com/fusiontables" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "fusiontables.v1.js")]
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

@subsection{table}
@defproc[(fusiontables-table-list
[#:pageToken pageToken string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of tables a user owns.

@racket[pageToken]: Continuation token specifying which result page to return. Optional.

@racket[maxResults]: Maximum number of styles to return. Optional. Default is 5.

}

@defproc[(fusiontables-table-get
[tableId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a specific table by its id.

@racket[tableId]: Identifier(ID) for the table being requested.

}

@defproc[(fusiontables-table-insert
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:tableId tableId string? 'N/A]
[#:columns columns string? 'N/A]
[#:attribution attribution string? 'N/A]
[#:attributionLink attributionLink string? 'N/A]
[#:baseTableIds baseTableIds string? 'N/A]
[#:isExportable isExportable string? 'N/A]
[#:sql sql string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new table.

@racket[kind]: Type name: a template for an individual table.

@racket[name]: Name assigned to a table.

@racket[description]: Optional description assigned to the table.

@racket[tableId]: Encrypted unique alphanumeric identifier for the table.

@racket[columns]: Columns in the table.

@racket[attribution]: Optional attribution assigned to the table.

@racket[attributionLink]: Optional link for attribution.

@racket[baseTableIds]: Optional base table identifier if this table is a view or merged table.

@racket[isExportable]: Variable for whether table is exportable.

@racket[sql]: Optional sql that encodes the table definition for derived tables.

}

@defproc[(fusiontables-table-patch
[tableId string?]
[#:replaceViewDefinition replaceViewDefinition string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:columns columns string? 'N/A]
[#:attribution attribution string? 'N/A]
[#:attributionLink attributionLink string? 'N/A]
[#:baseTableIds baseTableIds string? 'N/A]
[#:isExportable isExportable string? 'N/A]
[#:sql sql string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing table. Unless explicitly requested, only the name, description, and attribution will be updated. This method supports patch semantics.

@racket[tableId]: ID of the table that is being updated.

@racket[replaceViewDefinition]: Should the view definition also be updated? The specified view definition replaces the existing one. Only a view can be updated with a new definition.

@racket[kind]: Type name: a template for an individual table.

@racket[name]: Name assigned to a table.

@racket[description]: Optional description assigned to the table.

@racket[columns]: Columns in the table.

@racket[attribution]: Optional attribution assigned to the table.

@racket[attributionLink]: Optional link for attribution.

@racket[baseTableIds]: Optional base table identifier if this table is a view or merged table.

@racket[isExportable]: Variable for whether table is exportable.

@racket[sql]: Optional sql that encodes the table definition for derived tables.

}

@defproc[(fusiontables-table-copy
[tableId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Copies a table.

@racket[tableId]: ID of the table that is being copied.

}

@defproc[(fusiontables-table-update
[tableId string?]
[#:replaceViewDefinition replaceViewDefinition string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:columns columns string? 'N/A]
[#:attribution attribution string? 'N/A]
[#:attributionLink attributionLink string? 'N/A]
[#:baseTableIds baseTableIds string? 'N/A]
[#:isExportable isExportable string? 'N/A]
[#:sql sql string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing table. Unless explicitly requested, only the name, description, and attribution will be updated.

@racket[tableId]: ID of the table that is being updated.

@racket[replaceViewDefinition]: Should the view definition also be updated? The specified view definition replaces the existing one. Only a view can be updated with a new definition.

@racket[kind]: Type name: a template for an individual table.

@racket[name]: Name assigned to a table.

@racket[description]: Optional description assigned to the table.

@racket[columns]: Columns in the table.

@racket[attribution]: Optional attribution assigned to the table.

@racket[attributionLink]: Optional link for attribution.

@racket[baseTableIds]: Optional base table identifier if this table is a view or merged table.

@racket[isExportable]: Variable for whether table is exportable.

@racket[sql]: Optional sql that encodes the table definition for derived tables.

}

@defproc[(fusiontables-table-delete
[tableId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes a table.

@racket[tableId]: ID of the table that is being deleted.

}

@subsection{import}
@defproc[(fusiontables-import-insert
[tableId string?]
[#:delimiter delimiter string? 'N/A]
[#:encoding encoding string? 'N/A]
[#:endLine endLine string? 'N/A]
[#:isStrict isStrict string? 'N/A]
[#:startLine startLine string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Import more rows into a table.

@racket[tableId]: The table into which new rows are being imported.

@racket[delimiter]: The delimiter used to separate cell values. Default is ','.

@racket[encoding]: The encoding of the content. Default is UTF-8.

@racket[endLine]: The index of the last line from which to start importing, exclusive. Thus, the number of imported lines is endLine - startLine. If this parameter is not provided, the file will be imported until the last line of the file. If endLine is negative, then it is equivalent to N + endLine, where N is the number of lines in the file.

@racket[isStrict]: Whether the CSV will be parsed strictly or not. Default is true.

@racket[startLine]: The index of the first line from which to start importing, inclusive. Default is 0.

}

@subsection{query}
@defproc[(fusiontables-query-sql
[sql string?]
[#:hdrs hdrs string? 'N/A]
[#:typed typed string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Executes an SQL SELECT/INSERT/UPDATE/DELETE/SHOW/DESCRIBE/CREATE statement.

@racket[sql]: An SQL SELECT/SHOW/DESCRIBE/INSERT/UPDATE/DELETE/CREATE statement.

@racket[hdrs]: Should column names be included (in the first row)?. Default is true.

@racket[typed]: Should typed values be returned in the (JSON) response -- numbers for numeric values and parsed geometries for KML values? Default is true.

}

@defproc[(fusiontables-query-sqlGet
[sql string?]
[#:hdrs hdrs string? 'N/A]
[#:typed typed string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Executes an SQL SELECT/SHOW/DESCRIBE statement.

@racket[sql]: An SQL SELECT/SHOW/DESCRIBE statement.

@racket[hdrs]: Should column names be included (in the first row)?. Default is true.

@racket[typed]: Should typed values be returned in the (JSON) response -- numbers for numeric values and parsed geometries for KML values? Default is true.

}

@subsection{style}
@defproc[(fusiontables-style-list
[tableId string?]
[#:pageToken pageToken string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of styles.

@racket[tableId]: Table whose styles are being listed

@racket[pageToken]: Continuation token specifying which result page to return. Optional.

@racket[maxResults]: Maximum number of styles to return. Optional. Default is 5.

}

@defproc[(fusiontables-style-get
[tableId string?]
[styleId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Gets a specific style.

@racket[tableId]: Table to which the requested style belongs

@racket[styleId]: Identifier (integer) for a specific style in a table

}

@defproc[(fusiontables-style-insert
[tableId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:isDefaultForTable isDefaultForTable string? 'N/A]
[#:markerOptions markerOptions string? 'N/A]
[#:polygonOptions polygonOptions string? 'N/A]
[#:polylineOptions polylineOptions string? 'N/A]
[#:styleId styleId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Adds a new style for the table.

@racket[tableId]: Table for which a new style is being added

@racket[kind]: Type name: an individual style setting. A StyleSetting contains the style defintions for points, lines, and polygons in a table. Since a table can have any one or all of them, a style definition can have point, line and polygon style definitions.

@racket[name]: Optional name for the style setting.

@racket[isDefaultForTable]: Is this the default style for the table.

@racket[markerOptions]: Style definition for points in the table.

@racket[polygonOptions]: Style definition for polygons in the table.

@racket[polylineOptions]: Style definition for lines in the table.

@racket[styleId]: Identifier for the style setting (unique only within tables).

}

@defproc[(fusiontables-style-patch
[tableId string?]
[styleId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:isDefaultForTable isDefaultForTable string? 'N/A]
[#:markerOptions markerOptions string? 'N/A]
[#:polygonOptions polygonOptions string? 'N/A]
[#:polylineOptions polylineOptions string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing style. This method supports patch semantics.

@racket[tableId]: Table whose style is being updated.

@racket[styleId]: Identifier (within a table) for the style being updated.

@racket[kind]: Type name: an individual style setting. A StyleSetting contains the style defintions for points, lines, and polygons in a table. Since a table can have any one or all of them, a style definition can have point, line and polygon style definitions.

@racket[name]: Optional name for the style setting.

@racket[isDefaultForTable]: Is this the default style for the table.

@racket[markerOptions]: Style definition for points in the table.

@racket[polygonOptions]: Style definition for polygons in the table.

@racket[polylineOptions]: Style definition for lines in the table.

}

@defproc[(fusiontables-style-update
[tableId string?]
[styleId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:isDefaultForTable isDefaultForTable string? 'N/A]
[#:markerOptions markerOptions string? 'N/A]
[#:polygonOptions polygonOptions string? 'N/A]
[#:polylineOptions polylineOptions string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing style.

@racket[tableId]: Table whose style is being updated.

@racket[styleId]: Identifier (within a table) for the style being updated.

@racket[kind]: Type name: an individual style setting. A StyleSetting contains the style defintions for points, lines, and polygons in a table. Since a table can have any one or all of them, a style definition can have point, line and polygon style definitions.

@racket[name]: Optional name for the style setting.

@racket[isDefaultForTable]: Is this the default style for the table.

@racket[markerOptions]: Style definition for points in the table.

@racket[polygonOptions]: Style definition for polygons in the table.

@racket[polylineOptions]: Style definition for lines in the table.

}

@defproc[(fusiontables-style-delete
[tableId string?]
[styleId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes a style.

@racket[tableId]: Table from which the style is being deleted

@racket[styleId]: Identifier (within a table) for the style being deleted

}

@subsection{template}
@defproc[(fusiontables-template-list
[tableId string?]
[#:pageToken pageToken string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of templates.

@racket[tableId]: Identifier for the table whose templates are being requested

@racket[pageToken]: Continuation token specifying which results page to return. Optional.

@racket[maxResults]: Maximum number of templates to return. Optional. Default is 5.

}

@defproc[(fusiontables-template-get
[tableId string?]
[templateId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a specific template by its id

@racket[tableId]: Table to which the template belongs

@racket[templateId]: Identifier for the template that is being requested

}

@defproc[(fusiontables-template-insert
[tableId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:body body string? 'N/A]
[#:isDefaultForTable isDefaultForTable string? 'N/A]
[#:automaticColumnNames automaticColumnNames string? 'N/A]
[#:templateId templateId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new template for the table.

@racket[tableId]: Table for which a new template is being created

@racket[kind]: Type name: a template for the info window contents. The template can either include an HTML body or a list of columns from which the template is computed automatically.

@racket[name]: Optional name assigned to a template.

@racket[body]: Body of the template. It contains HTML with {column_name} to insert values from a particular column. The body is sanitized to remove certain tags, e.g., script. Only one of body or automaticColumns can be specified.

@racket[isDefaultForTable]: Is this the default template for the table.

@racket[automaticColumnNames]: List of columns from which the template is to be automatically constructed. Only one of body or automaticColumns can be specified.

@racket[templateId]: Identifier for the template, unique within the context of a particular table.

}

@defproc[(fusiontables-template-patch
[tableId string?]
[templateId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:body body string? 'N/A]
[#:isDefaultForTable isDefaultForTable string? 'N/A]
[#:automaticColumnNames automaticColumnNames string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing template. This method supports patch semantics.

@racket[tableId]: Table to which the updated template belongs

@racket[templateId]: Identifier for the template that is being updated

@racket[kind]: Type name: a template for the info window contents. The template can either include an HTML body or a list of columns from which the template is computed automatically.

@racket[name]: Optional name assigned to a template.

@racket[body]: Body of the template. It contains HTML with {column_name} to insert values from a particular column. The body is sanitized to remove certain tags, e.g., script. Only one of body or automaticColumns can be specified.

@racket[isDefaultForTable]: Is this the default template for the table.

@racket[automaticColumnNames]: List of columns from which the template is to be automatically constructed. Only one of body or automaticColumns can be specified.

}

@defproc[(fusiontables-template-update
[tableId string?]
[templateId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:body body string? 'N/A]
[#:isDefaultForTable isDefaultForTable string? 'N/A]
[#:automaticColumnNames automaticColumnNames string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an existing template

@racket[tableId]: Table to which the updated template belongs

@racket[templateId]: Identifier for the template that is being updated

@racket[kind]: Type name: a template for the info window contents. The template can either include an HTML body or a list of columns from which the template is computed automatically.

@racket[name]: Optional name assigned to a template.

@racket[body]: Body of the template. It contains HTML with {column_name} to insert values from a particular column. The body is sanitized to remove certain tags, e.g., script. Only one of body or automaticColumns can be specified.

@racket[isDefaultForTable]: Is this the default template for the table.

@racket[automaticColumnNames]: List of columns from which the template is to be automatically constructed. Only one of body or automaticColumns can be specified.

}

@defproc[(fusiontables-template-delete
[tableId string?]
[templateId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes a template

@racket[tableId]: Table from which the template is being deleted

@racket[templateId]: Identifier for the template which is being deleted

}

@subsection{column}
@defproc[(fusiontables-column-list
[tableId string?]
[#:pageToken pageToken string? 'N/A]
[#:maxResults maxResults string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of columns.

@racket[tableId]: Table whose columns are being listed.

@racket[pageToken]: Continuation token specifying which result page to return. Optional.

@racket[maxResults]: Maximum number of columns to return. Optional. Default is 5.

}

@defproc[(fusiontables-column-get
[tableId string?]
[columnId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a specific column by its id.

@racket[tableId]: Table to which the column belongs.

@racket[columnId]: Name or identifier for the column that is being requested.

}

@defproc[(fusiontables-column-insert
[tableId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:columnId columnId string? 'N/A]
[#:baseColumn baseColumn string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Adds a new column to the table.

@racket[tableId]: Table for which a new column is being added.

@racket[kind]: Type name: a template for an individual column.

@racket[name]: Required name of the column.

@racket[type]: Required type of the column.

@racket[columnId]: Identifier for the column.

@racket[baseColumn]: Optional identifier of the base column. If present, this column is derived from the specified base column.

}

@defproc[(fusiontables-column-patch
[tableId string?]
[columnId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:baseColumn baseColumn string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the name or type of an existing column. This method supports patch semantics.

@racket[tableId]: Table for which the column is being updated.

@racket[columnId]: Name or identifier for the column that is being updated.

@racket[kind]: Type name: a template for an individual column.

@racket[name]: Required name of the column.

@racket[type]: Required type of the column.

@racket[baseColumn]: Optional identifier of the base column. If present, this column is derived from the specified base column.

}

@defproc[(fusiontables-column-update
[tableId string?]
[columnId string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:baseColumn baseColumn string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the name or type of an existing column.

@racket[tableId]: Table for which the column is being updated.

@racket[columnId]: Name or identifier for the column that is being updated.

@racket[kind]: Type name: a template for an individual column.

@racket[name]: Required name of the column.

@racket[type]: Required type of the column.

@racket[baseColumn]: Optional identifier of the base column. If present, this column is derived from the specified base column.

}

@defproc[(fusiontables-column-delete
[tableId string?]
[columnId string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the column.

@racket[tableId]: Table from which the column is being deleted.

@racket[columnId]: Name or identifier for the column being deleted.

}

