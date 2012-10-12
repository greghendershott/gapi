{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "bigquery:v2",
 "name": "bigquery",
 "version": "v2",
 "revision": "20121008",
 "title": "BigQuery API",
 "description": "A data platform for customers to create, manage, share and query data.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/search-16.gif",
  "x32": "http://www.google.com/images/icons/product/search-32.gif"
 },
 "documentationLink": "https://developers.google.com/bigquery/docs/overview",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/bigquery/v2/",
 "basePath": "/bigquery/v2/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "bigquery/v2/",
 "batchPath": "batch",
 "parameters": {
  "alt": {
   "type": "string",
   "description": "Data format for the response.",
   "default": "json",
   "enum": [
    "csv",
    "json"
   ],
   "enumDescriptions": [
    "Responses with Content-Type of text/csv",
    "Responses with Content-Type of application/json"
   ],
   "location": "query"
  },
  "fields": {
   "type": "string",
   "description": "Selector specifying which fields to include in a partial response.",
   "location": "query"
  },
  "key": {
   "type": "string",
   "description": "API key. Your API key identifies your project and provides you with API access, quota, and reports. Required unless you provide an OAuth 2.0 token.",
   "location": "query"
  },
  "oauth_token": {
   "type": "string",
   "description": "OAuth 2.0 token for the current user.",
   "location": "query"
  },
  "prettyPrint": {
   "type": "boolean",
   "description": "Returns response with indentations and line breaks.",
   "default": "true",
   "location": "query"
  },
  "quotaUser": {
   "type": "string",
   "description": "Available to use for quota purposes for server-side applications. Can be any arbitrary string assigned to a user, but should not exceed 40 characters. Overrides userIp if both are provided.",
   "location": "query"
  },
  "userIp": {
   "type": "string",
   "description": "IP address of the site where the request originates. Use this if you want to enforce per-user limits.",
   "location": "query"
  }
 },
 "auth": {
  "oauth2": {
   "scopes": {
    "https://www.googleapis.com/auth/bigquery": {
     "description": "View and manage your data in Google BigQuery"
    },
    "https://www.googleapis.com/auth/devstorage.full_control": {
     "description": "Manage your data and permissions in Google Cloud Storage"
    },
    "https://www.googleapis.com/auth/devstorage.read_only": {
     "description": "View your data in Google Cloud Storage"
    },
    "https://www.googleapis.com/auth/devstorage.read_write": {
     "description": "Manage your data in Google Cloud Storage"
    }
   }
  }
 },
 "schemas": {
  "Dataset": {
   "id": "Dataset",
   "type": "object",
   "properties": {
    "access": {
     "type": "array",
     "description": "[Optional] Describes users' rights on the dataset. You can assign the same role to multiple users, and assign multiple roles to the same user.\nDefault values assigned to a new dataset are as follows: OWNER - Project owners, dataset creator READ - Project readers WRITE - Project writers\nSee ACLs and Rights for a description of these rights. If you specify any of these roles when creating a dataset, the assigned roles will overwrite the defaults listed above.\nTo revoke rights to a dataset, call datasets.update() and omit the names of anyone whose rights you wish to revoke. However, every dataset must have at least one entity granted OWNER role.\nEach access object can have only one of the following members: userByEmail, groupByEmail, domain, or allAuthenticatedUsers.",
     "items": {
      "type": "object",
      "properties": {
       "domain": {
        "type": "string",
        "description": "[Pick one] A domain to grant access to. Any users signed in with the domain specified will be granted the specified access. Example: \"example.com\"."
       },
       "groupByEmail": {
        "type": "string",
        "description": "[Pick one] A fully-qualified email address of a mailing list to grant access to. This must be either a Google Groups mailing list (ends in @googlegroups.com) or a group managed by an enterprise version of Google Groups."
       },
       "role": {
        "type": "string",
        "description": "[Required] Describes the rights granted to the user specified by the other member of the access object. The following string values are supported: READ - User can call any list() or get() method on any collection or resource. WRITE - User can call any method on any collection except for datasets, on which they can call list() and get(). OWNER - User can call any method. The dataset creator is granted this role by default."
       },
       "specialGroup": {
        "type": "string",
        "description": "[Pick one] A special group to grant access to. The valid values are: projectOwners: Owners of the enclosing project. projectReaders: Readers of the enclosing project. projectWriters: Writers of the enclosing project. allAuthenticatedUsers: All authenticated BigQuery users."
       },
       "userByEmail": {
        "type": "string",
        "description": "[Pick one] A fully qualified email address of a user to grant access to. For example: fred@example.com."
       }
      }
     }
    },
    "creationTime": {
     "type": "string",
     "description": "[Output-only] The time when this dataset was created, in milliseconds since the epoch.",
     "format": "int64"
    },
    "datasetReference": {
     "$ref": "DatasetReference",
     "description": "[Required] Reference identifying dataset."
    },
    "description": {
     "type": "string",
     "description": "[Optional] A user-friendly string description for the dataset. This might be shown in BigQuery UI for browsing the dataset."
    },
    "etag": {
     "type": "string",
     "description": "[Output-only] A hash of this resource."
    },
    "friendlyName": {
     "type": "string",
     "description": "[Optional] A descriptive name for this dataset, which might be shown in any BigQuery user interfaces for browsing the dataset. Use datasetId for making API calls."
    },
    "id": {
     "type": "string",
     "description": "[Output-only] The fully-qualified unique name of this dataset in the format projectId:datasetId. The dataset name without the project name is given in the datasetId field. When creating a new dataset, leave this field blank, and instead specify the datasetId field."
    },
    "kind": {
     "type": "string",
     "description": "[Output-only] The resource type.",
     "default": "bigquery#dataset"
    },
    "lastModifiedTime": {
     "type": "string",
     "description": "[Output-only] The date when this dataset or any of its tables was last modified, in milliseconds since the epoch.",
     "format": "int64"
    },
    "selfLink": {
     "type": "string",
     "description": "[Output-only] An URL that can be used to access this resource again. You can use this URL in Get or Update requests to this resource."
    }
   }
  },
  "DatasetList": {
   "id": "DatasetList",
   "type": "object",
   "properties": {
    "datasets": {
     "type": "array",
     "description": "An array of one or more summarized dataset resources. Absent when there are no datasets in the specified project.",
     "items": {
      "type": "object",
      "properties": {
       "datasetReference": {
        "$ref": "DatasetReference",
        "description": "Reference identifying dataset."
       },
       "friendlyName": {
        "type": "string",
        "description": "A descriptive name for this dataset, if one exists."
       },
       "id": {
        "type": "string",
        "description": "The fully-qualified unique name of this dataset in the format projectId:datasetId."
       },
       "kind": {
        "type": "string",
        "description": "The resource type.",
        "default": "bigquery#dataset"
       }
      }
     }
    },
    "etag": {
     "type": "string",
     "description": "A hash of this page of results. See Paging Through Results in the developer's guide."
    },
    "kind": {
     "type": "string",
     "description": "The type of list.",
     "default": "bigquery#datasetList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token to request the next page of results. Present only when there is more than one page of results.* See Paging Through Results in the developer's guide."
    }
   }
  },
  "DatasetReference": {
   "id": "DatasetReference",
   "type": "object",
   "properties": {
    "datasetId": {
     "type": "string",
     "description": "[Required] A unique ID for this dataset, without the project name.",
     "annotations": {
      "required": [
       "bigquery.datasets.update"
      ]
     }
    },
    "projectId": {
     "type": "string",
     "description": "[Optional] The ID of the container project.",
     "annotations": {
      "required": [
       "bigquery.datasets.update"
      ]
     }
    }
   }
  },
  "ErrorProto": {
   "id": "ErrorProto",
   "type": "object",
   "properties": {
    "debugInfo": {
     "type": "string",
     "description": "Debugging information for the service, if present. Should be ignored."
    },
    "location": {
     "type": "string",
     "description": "Specifies where the error occurred, if present."
    },
    "message": {
     "type": "string",
     "description": "A human readable explanation of the error."
    },
    "reason": {
     "type": "string",
     "description": "Specifies the error reason. For example, reason will be \"required\" or \"invalid\" if some field was missing or malformed."
    }
   }
  },
  "GetQueryResultsResponse": {
   "id": "GetQueryResultsResponse",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "A hash of this response."
    },
    "jobComplete": {
     "type": "boolean",
     "description": "Whether the query has completed or not. If rows or totalRows are present, this will always be true. If this is false, totalRows will not be available."
    },
    "jobReference": {
     "$ref": "JobReference",
     "description": "Reference to the BigQuery Job that was created to run the query. This field will be present even if the original request timed out, in which case GetQueryResults can be used to read the results once the query has completed. Since this API only returns the first page of results, subsequent pages can be fetched via the same mechanism (GetQueryResults)."
    },
    "kind": {
     "type": "string",
     "description": "The resource type of the response.",
     "default": "bigquery#getQueryResultsResponse"
    },
    "rows": {
     "type": "array",
     "description": "An object with as many results as can be contained within the maximum permitted reply size. To get any additional rows, you can call GetQueryResults and specify the jobReference returned above. Present only when the query completes successfully.",
     "items": {
      "$ref": "TableRow"
     }
    },
    "schema": {
     "$ref": "TableSchema",
     "description": "The schema of the results. Present only when the query completes successfully."
    },
    "totalRows": {
     "type": "string",
     "description": "The total number of rows in the complete query result set, which can be more than the number of rows in this single page of results. Present only when the query completes successfully.",
     "format": "uint64"
    }
   }
  },
  "Job": {
   "id": "Job",
   "type": "object",
   "properties": {
    "configuration": {
     "$ref": "JobConfiguration",
     "description": "[Required] Describes the job configuration."
    },
    "etag": {
     "type": "string",
     "description": "[Output-only] A hash of this resource."
    },
    "id": {
     "type": "string",
     "description": "[Output-only] Opaque ID field of the job"
    },
    "jobReference": {
     "$ref": "JobReference",
     "description": "[Optional] Reference describing the unique-per-user name of the job."
    },
    "kind": {
     "type": "string",
     "description": "[Output-only] The type of the resource.",
     "default": "bigquery#job"
    },
    "selfLink": {
     "type": "string",
     "description": "[Output-only] A URL that can be used to access this resource again."
    },
    "statistics": {
     "$ref": "JobStatistics",
     "description": "[Output-only] Information about the job, including starting time and ending time of the job."
    },
    "status": {
     "$ref": "JobStatus",
     "description": "[Output-only] The status of this job. Examine this value when polling an asynchronous job to see if the job is complete."
    }
   }
  },
  "JobConfiguration": {
   "id": "JobConfiguration",
   "type": "object",
   "properties": {
    "copy": {
     "$ref": "JobConfigurationTableCopy",
     "description": "[Pick one] Copies a table."
    },
    "extract": {
     "$ref": "JobConfigurationExtract",
     "description": "[Pick one] Configures an extract job."
    },
    "link": {
     "$ref": "JobConfigurationLink",
     "description": "[Pick one] Configures a link job."
    },
    "load": {
     "$ref": "JobConfigurationLoad",
     "description": "[Pick one] Configures a load job."
    },
    "properties": {
     "$ref": "JobConfigurationProperties",
     "description": "[Optional] Properties providing extra details about how the job should be run. Not used for most jobs."
    },
    "query": {
     "$ref": "JobConfigurationQuery",
     "description": "[Pick one] Configures a query job."
    }
   }
  },
  "JobConfigurationExtract": {
   "id": "JobConfigurationExtract",
   "type": "object",
   "properties": {
    "destinationFormat": {
     "type": "string",
     "description": "[Experimental] Optional and defaults to CSV. Format with which files should be exported. To export to CSV, specify \"CSV\". Tables with nested or repeated fields cannot be exported as CSV. To export to newline-delimited JSON, specify \"NEWLINE_DELIMITED_JSON\"."
    },
    "destinationUri": {
     "type": "string",
     "description": "[Required] The fully-qualified Google Cloud Storage URI where the extracted table should be written."
    },
    "fieldDelimiter": {
     "type": "string",
     "description": "[Optional] Delimiter to use between fields in the exported data. Default is ','"
    },
    "printHeader": {
     "type": "boolean",
     "description": "[Optional] Whether to print out a heder row in the results. Default is true."
    },
    "sourceTable": {
     "$ref": "TableReference",
     "description": "[Required] A reference to the table being exported."
    }
   }
  },
  "JobConfigurationLink": {
   "id": "JobConfigurationLink",
   "type": "object",
   "properties": {
    "createDisposition": {
     "type": "string",
     "description": "[Optional] Whether or not to create a new table, if none exists."
    },
    "destinationTable": {
     "$ref": "TableReference",
     "description": "[Required] The destination table of the link job."
    },
    "sourceUri": {
     "type": "array",
     "description": "[Required] URI of source table to link.",
     "items": {
      "type": "string"
     }
    },
    "writeDisposition": {
     "type": "string",
     "description": "[Optional] Whether to overwrite an existing table (WRITE_TRUNCATE), append to an existing table (WRITE_APPEND), or require that the the table is empty (WRITE_EMPTY). Default is WRITE_APPEND."
    }
   }
  },
  "JobConfigurationLoad": {
   "id": "JobConfigurationLoad",
   "type": "object",
   "properties": {
    "allowQuotedNewlines": {
     "type": "boolean",
     "description": "[Experimental] Whether to allow quoted newlines in the source CSV data."
    },
    "createDisposition": {
     "type": "string",
     "description": "[Optional] Whether to create the table if it doesn't already exist (CREATE_IF_NEEDED) or to require the table already exist (CREATE_NEVER). Default is CREATE_IF_NEEDED."
    },
    "destinationTable": {
     "$ref": "TableReference",
     "description": "[Required] Table being written to."
    },
    "encoding": {
     "type": "string",
     "description": "[Optional] Character encoding of the input data. May be UTF-8 or ISO-8859-1. Default is UTF-8."
    },
    "fieldDelimiter": {
     "type": "string",
     "description": "[Optional] Delimiter to use between fields in the import data. Default is ','. Note that delimiters are applied to the raw, binary data before the encoding is applied."
    },
    "maxBadRecords": {
     "type": "integer",
     "description": "[Optional] Maximum number of bad records that should be ignored before the entire job is aborted and no updates are performed.",
     "format": "int32"
    },
    "quote": {
     "type": "string",
     "description": "[Optional] Quote character to use. Default is '\"'. Note that quoting is done on the raw, binary data before the encoding is applied. If no quoting is done, use am empty string."
    },
    "schema": {
     "$ref": "TableSchema",
     "description": "[Optional] Schema of the table being written to."
    },
    "schemaInline": {
     "type": "string",
     "description": "[Experimental] Inline schema. For CSV schemas, specify as \"Field1:Type1[,Field2:Type2]*\". For example, \"foo:STRING, bar:INTEGER, baz:FLOAT\""
    },
    "schemaInlineFormat": {
     "type": "string",
     "description": "[Experimental] Format of inlineSchema field."
    },
    "skipLeadingRows": {
     "type": "integer",
     "description": "[Optional] Number of rows of initial data to skip in the data being imported.",
     "format": "int32"
    },
    "sourceFormat": {
     "type": "string",
     "description": "[Experimental] Optional and defaults to CSV. Format of source files. For CSV uploads, specify \"CSV\". For imports of datastore backups, specify \"DATASTORE_BACKUP\". For imports of newline-delimited JSON, specify \"NEWLINE_DELIMITED_JSON\"."
    },
    "sourceUris": {
     "type": "array",
     "description": "[Required] Source URIs describing Google Cloud Storage locations of data to load.",
     "items": {
      "type": "string"
     }
    },
    "writeDisposition": {
     "type": "string",
     "description": "[Optional] Whether to overwrite an existing table (WRITE_TRUNCATE), append to an existing table (WRITE_APPEND), or require that the the table is empty (WRITE_EMPTY). Default is WRITE_APPEND."
    }
   }
  },
  "JobConfigurationProperties": {
   "id": "JobConfigurationProperties",
   "type": "object",
   "additionalProperties": {
    "type": "string",
    "description": "Key-value property pairs."
   }
  },
  "JobConfigurationQuery": {
   "id": "JobConfigurationQuery",
   "type": "object",
   "properties": {
    "createDisposition": {
     "type": "string",
     "description": "[Optional] Whether to create the table if it doesn't already exist (CREATE_IF_NEEDED) or to require the table already exist (CREATE_NEVER). Default is CREATE_IF_NEEDED."
    },
    "defaultDataset": {
     "$ref": "DatasetReference",
     "description": "[Optional] Specifies the default dataset to assume for unqualified table names in the query."
    },
    "destinationTable": {
     "$ref": "TableReference",
     "description": "[Optional] Describes the table where the query results should be stored. If not present, a new table will be created to store the results."
    },
    "priority": {
     "type": "string",
     "description": "[Optional] Specifies a priority for the query. Default is INTERACTIVE. Alternative is BATCH."
    },
    "query": {
     "type": "string",
     "description": "[Required] BigQuery SQL query to execute."
    },
    "writeDisposition": {
     "type": "string",
     "description": "[Optional] Whether to overwrite an existing table (WRITE_TRUNCATE), append to an existing table (WRITE_APPEND), or require that the the table is empty (WRITE_EMPTY). Default is WRITE_EMPTY."
    }
   }
  },
  "JobConfigurationTableCopy": {
   "id": "JobConfigurationTableCopy",
   "type": "object",
   "properties": {
    "createDisposition": {
     "type": "string",
     "description": "[Optional] Whether or not to create a new table, if none exists."
    },
    "destinationTable": {
     "$ref": "TableReference",
     "description": "[Required] The destination table"
    },
    "sourceTable": {
     "$ref": "TableReference",
     "description": "[Required] Source table to copy."
    },
    "writeDisposition": {
     "type": "string",
     "description": "[Optional] Whether or not to append or require the table to be empty."
    }
   }
  },
  "JobList": {
   "id": "JobList",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "A hash of this page of results."
    },
    "jobs": {
     "type": "array",
     "description": "List of jobs that were requested.",
     "items": {
      "type": "object",
      "properties": {
       "configuration": {
        "$ref": "JobConfiguration",
        "description": "[Full-projection-only] Specifies the job configuration."
       },
       "errorResult": {
        "$ref": "ErrorProto",
        "description": "A result object that will be present only if the job has failed."
       },
       "id": {
        "type": "string",
        "description": "Unique opaque ID of the job."
       },
       "jobReference": {
        "$ref": "JobReference",
        "description": "Job reference uniquely identifying the job."
       },
       "kind": {
        "type": "string",
        "description": "The resource type.",
        "default": "bigquery#job"
       },
       "state": {
        "type": "string",
        "description": "Running state of the job. When the state is DONE, errorResult can be checked to determine whether the job succeeded or failed."
       },
       "statistics": {
        "$ref": "JobStatistics",
        "description": "[Output-only] Information about the job, including starting time and ending time of the job."
       },
       "status": {
        "$ref": "JobStatus",
        "description": "[Full-projection-only] Describes the state of the job."
       }
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "The resource type of the response.",
     "default": "bigquery#jobList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token to request the next page of results."
    },
    "totalItems": {
     "type": "integer",
     "description": "Total number of jobs in this collection.",
     "format": "int32"
    }
   }
  },
  "JobReference": {
   "id": "JobReference",
   "type": "object",
   "properties": {
    "jobId": {
     "type": "string",
     "description": "[Required] ID of the job.",
     "annotations": {
      "required": [
       "bigquery.jobs.getQueryResults"
      ]
     }
    },
    "projectId": {
     "type": "string",
     "description": "[Required] Project ID being billed for the job.",
     "annotations": {
      "required": [
       "bigquery.jobs.getQueryResults"
      ]
     }
    }
   }
  },
  "JobStatistics": {
   "id": "JobStatistics",
   "type": "object",
   "properties": {
    "endTime": {
     "type": "string",
     "description": "[Output-only] End time of this job, in milliseconds since the epoch.",
     "format": "int64"
    },
    "load": {
     "$ref": "JobStatistics3",
     "description": "[Output-only] Statistics for a load job."
    },
    "query": {
     "$ref": "JobStatistics2",
     "description": "[Output-only] Statistics for a query job."
    },
    "startTime": {
     "type": "string",
     "description": "[Output-only] Start time of this job, in milliseconds since the epoch.",
     "format": "int64"
    },
    "totalBytesProcessed": {
     "type": "string",
     "description": "[Output-only] [Deprecated] Use the bytes processed in the query statistics instead.",
     "format": "int64"
    }
   }
  },
  "JobStatistics2": {
   "id": "JobStatistics2",
   "type": "object",
   "properties": {
    "totalBytesProcessed": {
     "type": "string",
     "description": "[Output-only] Total bytes processed for this job.",
     "format": "int64"
    }
   }
  },
  "JobStatistics3": {
   "id": "JobStatistics3",
   "type": "object",
   "properties": {
    "inputFileBytes": {
     "type": "string",
     "description": "[Output-only] Number of bytes of source data in a joad job.",
     "format": "int64"
    },
    "inputFiles": {
     "type": "string",
     "description": "[Output-only] Number of source files in a load job.",
     "format": "int64"
    },
    "outputBytes": {
     "type": "string",
     "description": "[Output-only] Size of the loaded data in bytes. Note that while an import job is in the running state, this value may change.",
     "format": "int64"
    },
    "outputRows": {
     "type": "string",
     "description": "[Output-only] Number of rows imported in a load job. Note that while an import job is in the running state, this value may change.",
     "format": "int64"
    }
   }
  },
  "JobStatus": {
   "id": "JobStatus",
   "type": "object",
   "properties": {
    "errorResult": {
     "$ref": "ErrorProto",
     "description": "[Output-only] Final error result of the job. If present, indicates that the job has completed and was unsuccessful."
    },
    "errors": {
     "type": "array",
     "description": "[Output-only] All errors encountered during the running of the job. Errors here do not necessarily mean that the job has completed or was unsuccessful.",
     "items": {
      "$ref": "ErrorProto"
     }
    },
    "state": {
     "type": "string",
     "description": "[Output-only] Running state of the job."
    }
   }
  },
  "ProjectList": {
   "id": "ProjectList",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "A hash of the page of results"
    },
    "kind": {
     "type": "string",
     "description": "The type of list.",
     "default": "bigquery#projectList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token to request the next page of results."
    },
    "projects": {
     "type": "array",
     "description": "Projects to which you have at least READ access.",
     "items": {
      "type": "object",
      "properties": {
       "friendlyName": {
        "type": "string",
        "description": "A descriptive name for this project."
       },
       "id": {
        "type": "string",
        "description": "An opaque ID of this project."
       },
       "kind": {
        "type": "string",
        "description": "The resource type.",
        "default": "bigquery#project"
       },
       "projectReference": {
        "$ref": "ProjectReference",
        "description": "A unique reference to this project."
       }
      }
     }
    },
    "totalItems": {
     "type": "integer",
     "description": "The total number of projects in the list.",
     "format": "int32"
    }
   }
  },
  "ProjectReference": {
   "id": "ProjectReference",
   "type": "object",
   "properties": {
    "projectId": {
     "type": "string",
     "description": "[Required] ID of the project. Can be either the numeric ID or the assigned ID of the project."
    }
   }
  },
  "QueryRequest": {
   "id": "QueryRequest",
   "type": "object",
   "properties": {
    "defaultDataset": {
     "$ref": "DatasetReference",
     "description": "[Optional] Specifies the default datasetId and projectId to assume for any unqualified table names in the query. If not set, all table names in the query string must be fully-qualified in the format projectId:datasetId.tableid."
    },
    "dryRun": {
     "type": "boolean",
     "description": "[Optional] If set, don't actually run the query. A valid query will return an empty response, while an invalid query will return the same error it would if it wasn't a dry run."
    },
    "kind": {
     "type": "string",
     "description": "The resource type of the request.",
     "default": "bigquery#queryRequest"
    },
    "maxResults": {
     "type": "integer",
     "description": "[Optional] The maximum number of results to return per page of results. If the response list exceeds the maximum response size for a single response, you will have to page through the results. Default is to return the maximum response size.",
     "format": "uint32"
    },
    "query": {
     "type": "string",
     "description": "[Required] A query string, following the BigQuery query syntax of the query to execute. Table names should be qualified by dataset name in the format projectId:datasetId.tableId unless you specify the defaultDataset value. If the table is in the same project as the job, you can omit the project ID. Example: SELECT f1 FROM myProjectId:myDatasetId.myTableId.",
     "annotations": {
      "required": [
       "bigquery.jobs.query"
      ]
     }
    },
    "timeoutMs": {
     "type": "integer",
     "description": "[Optional] How long to wait for the query to complete, in milliseconds, before returning. Default is to return immediately. If the timeout passes before the job completes, the request will fail with a TIMEOUT error.",
     "format": "uint32"
    }
   }
  },
  "QueryResponse": {
   "id": "QueryResponse",
   "type": "object",
   "properties": {
    "jobComplete": {
     "type": "boolean",
     "description": "Whether the query has completed or not. If rows or totalRows are present, this will always be true. If this is false, totalRows will not be available."
    },
    "jobReference": {
     "$ref": "JobReference",
     "description": "Reference to the Job that was created to run the query. This field will be present even if the original request timed out, in which case GetQueryResults can be used to read the results once the query has completed. Since this API only returns the first page of results, subsequent pages can be fetched via the same mechanism (GetQueryResults)."
    },
    "kind": {
     "type": "string",
     "description": "The resource type.",
     "default": "bigquery#queryResponse"
    },
    "rows": {
     "type": "array",
     "description": "An object with as many results as can be contained within the maximum permitted reply size. To get any additional rows, you can call GetQueryResults and specify the jobReference returned above.",
     "items": {
      "$ref": "TableRow"
     }
    },
    "schema": {
     "$ref": "TableSchema",
     "description": "The schema of the results. Present only when the query completes successfully."
    },
    "totalRows": {
     "type": "string",
     "description": "The total number of rows in the complete query result set, which can be more than the number of rows in this single page of results.",
     "format": "uint64"
    }
   }
  },
  "Table": {
   "id": "Table",
   "type": "object",
   "properties": {
    "creationTime": {
     "type": "string",
     "description": "[Output-only] The time when this table was created, in milliseconds since the epoch.",
     "format": "int64"
    },
    "description": {
     "type": "string",
     "description": "[Optional] A user-friendly description of this table."
    },
    "etag": {
     "type": "string",
     "description": "[Output-only] A hash of this resource."
    },
    "expirationTime": {
     "type": "string",
     "description": "[Optional] The time when this table expires, in milliseconds since the epoch. If not present, the table will persist indefinitely. Expired tables will be deleted and their storage reclaimed.",
     "format": "int64"
    },
    "friendlyName": {
     "type": "string",
     "description": "[Optional] A descriptive name for this table."
    },
    "id": {
     "type": "string",
     "description": "[Output-only] An opaque ID uniquely identifying the table."
    },
    "kind": {
     "type": "string",
     "description": "[Output-only] The type of the resource.",
     "default": "bigquery#table"
    },
    "lastModifiedTime": {
     "type": "string",
     "description": "[Output-only] The time when this table was last modified, in milliseconds since the epoch.",
     "format": "int64"
    },
    "numBytes": {
     "type": "string",
     "description": "[Output-only] The size of the table in bytes.",
     "format": "int64"
    },
    "numRows": {
     "type": "string",
     "description": "[Output-only] The number of rows of data in this table.",
     "format": "uint64"
    },
    "schema": {
     "$ref": "TableSchema",
     "description": "[Optional] Describes the schema of this table."
    },
    "selfLink": {
     "type": "string",
     "description": "[Output-only] A URL that can be used to access this resource again."
    },
    "tableReference": {
     "$ref": "TableReference",
     "description": "[Required] Reference describing the ID of this table."
    }
   }
  },
  "TableCell": {
   "id": "TableCell",
   "type": "object",
   "description": "Represents a single cell in the result set.",
   "properties": {
    "v": {
     "type": "any"
    }
   }
  },
  "TableDataList": {
   "id": "TableDataList",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "A hash of this page of results."
    },
    "kind": {
     "type": "string",
     "description": "The resource type of the response.",
     "default": "bigquery#tableDataList"
    },
    "pageToken": {
     "type": "string",
     "description": "A token used for paging results. Providing this token instead of the startRow parameter can help you retrieve stable results when an underlying table is changing."
    },
    "rows": {
     "type": "array",
     "description": "Rows of results.",
     "items": {
      "$ref": "TableRow"
     }
    },
    "totalRows": {
     "type": "string",
     "description": "The total number of rows in the complete table.",
     "format": "int64"
    }
   }
  },
  "TableFieldSchema": {
   "id": "TableFieldSchema",
   "type": "object",
   "properties": {
    "fields": {
     "type": "array",
     "description": "[Optional] Describes nested fields when type is RECORD.",
     "items": {
      "$ref": "TableFieldSchema"
     }
    },
    "mode": {
     "type": "string",
     "description": "[Optional] Mode of the field (whether or not it can be null. Default is NULLABLE."
    },
    "name": {
     "type": "string",
     "description": "[Required] Name of the field."
    },
    "type": {
     "type": "string",
     "description": "[Required] Data type of the field."
    }
   }
  },
  "TableList": {
   "id": "TableList",
   "type": "object",
   "properties": {
    "etag": {
     "type": "string",
     "description": "A hash of this page of results."
    },
    "kind": {
     "type": "string",
     "description": "The type of list.",
     "default": "bigquery#tableList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token to request the next page of results."
    },
    "tables": {
     "type": "array",
     "description": "Tables in the requested dataset.",
     "items": {
      "type": "object",
      "properties": {
       "friendlyName": {
        "type": "string",
        "description": "The user-friendly name for this table."
       },
       "id": {
        "type": "string",
        "description": "An opaque ID of the table"
       },
       "kind": {
        "type": "string",
        "description": "The resource type.",
        "default": "bigquery#table"
       },
       "tableReference": {
        "$ref": "TableReference",
        "description": "A reference uniquely identifying the table."
       }
      }
     }
    },
    "totalItems": {
     "type": "integer",
     "description": "The total number of tables in the dataset.",
     "format": "int32"
    }
   }
  },
  "TableReference": {
   "id": "TableReference",
   "type": "object",
   "properties": {
    "datasetId": {
     "type": "string",
     "description": "[Required] ID of the dataset containing the table.",
     "annotations": {
      "required": [
       "bigquery.tables.update"
      ]
     }
    },
    "projectId": {
     "type": "string",
     "description": "[Required] ID of the project billed for storage of the table.",
     "annotations": {
      "required": [
       "bigquery.tables.update"
      ]
     }
    },
    "tableId": {
     "type": "string",
     "description": "[Required] ID of the table.",
     "annotations": {
      "required": [
       "bigquery.tables.update"
      ]
     }
    }
   }
  },
  "TableRow": {
   "id": "TableRow",
   "type": "object",
   "description": "Represents a single row in the result set, consisting of one or more fields.",
   "properties": {
    "f": {
     "type": "array",
     "items": {
      "$ref": "TableCell"
     }
    }
   }
  },
  "TableSchema": {
   "id": "TableSchema",
   "type": "object",
   "properties": {
    "fields": {
     "type": "array",
     "description": "Describes the fields in a table.",
     "items": {
      "$ref": "TableFieldSchema"
     }
    }
   }
  }
 },
 "resources": {
  "datasets": {
   "methods": {
    "delete": {
     "id": "bigquery.datasets.delete",
     "path": "projects/{projectId}/datasets/{datasetId}",
     "httpMethod": "DELETE",
     "description": "Deletes the dataset specified by datasetId value. Before you can delete a dataset, you must delete all its tables, either manually or by specifying deleteContents. Immediately after deletion, you can create another dataset with the same name.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of dataset being deleted",
       "required": true,
       "location": "path"
      },
      "deleteContents": {
       "type": "boolean",
       "description": "If True, delete all the tables in the dataset. If False and the dataset contains tables, the request will fail. Default is False",
       "location": "query"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the dataset being deleted",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "get": {
     "id": "bigquery.datasets.get",
     "path": "projects/{projectId}/datasets/{datasetId}",
     "httpMethod": "GET",
     "description": "Returns the dataset specified by datasetID.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the requested dataset",
       "required": true,
       "location": "path"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the requested dataset",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId"
     ],
     "response": {
      "$ref": "Dataset"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "insert": {
     "id": "bigquery.datasets.insert",
     "path": "projects/{projectId}/datasets",
     "httpMethod": "POST",
     "description": "Creates a new empty dataset.",
     "parameters": {
      "projectId": {
       "type": "string",
       "description": "Project ID of the new dataset",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId"
     ],
     "request": {
      "$ref": "Dataset"
     },
     "response": {
      "$ref": "Dataset"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "list": {
     "id": "bigquery.datasets.list",
     "path": "projects/{projectId}/datasets",
     "httpMethod": "GET",
     "description": "Lists all the datasets in the specified project to which the caller has read access; however, a project owner can list (but not necessarily get) all datasets in his project.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of results to return",
       "format": "uint32",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Page token, returned by a previous call, to request the next page of results",
       "location": "query"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the datasets to be listed",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId"
     ],
     "response": {
      "$ref": "DatasetList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "patch": {
     "id": "bigquery.datasets.patch",
     "path": "projects/{projectId}/datasets/{datasetId}",
     "httpMethod": "PATCH",
     "description": "Updates information in an existing dataset, specified by datasetId. Properties not included in the submitted resource will not be changed. If you include the access property without any values assigned, the request will fail as you must specify at least one owner for a dataset. This method supports patch semantics.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the dataset being updated",
       "required": true,
       "location": "path"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the dataset being updated",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId"
     ],
     "request": {
      "$ref": "Dataset"
     },
     "response": {
      "$ref": "Dataset"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "update": {
     "id": "bigquery.datasets.update",
     "path": "projects/{projectId}/datasets/{datasetId}",
     "httpMethod": "PUT",
     "description": "Updates information in an existing dataset, specified by datasetId. Properties not included in the submitted resource will not be changed. If you include the access property without any values assigned, the request will fail as you must specify at least one owner for a dataset.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the dataset being updated",
       "required": true,
       "location": "path"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the dataset being updated",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId"
     ],
     "request": {
      "$ref": "Dataset"
     },
     "response": {
      "$ref": "Dataset"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    }
   }
  },
  "jobs": {
   "methods": {
    "get": {
     "id": "bigquery.jobs.get",
     "path": "projects/{projectId}/jobs/{jobId}",
     "httpMethod": "GET",
     "description": "Retrieves the specified job by ID.",
     "parameters": {
      "jobId": {
       "type": "string",
       "description": "Job ID of the requested job",
       "required": true,
       "location": "path"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the requested job",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "jobId"
     ],
     "response": {
      "$ref": "Job"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "getQueryResults": {
     "id": "bigquery.jobs.getQueryResults",
     "path": "projects/{projectId}/queries/{jobId}",
     "httpMethod": "GET",
     "description": "Retrieves the results of a query job.",
     "parameters": {
      "jobId": {
       "type": "string",
       "description": "Job ID of the query job",
       "required": true,
       "location": "path"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to read",
       "format": "uint32",
       "location": "query"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the query job",
       "required": true,
       "location": "path"
      },
      "startIndex": {
       "type": "string",
       "description": "Zero-based index of the starting row",
       "format": "uint64",
       "location": "query"
      },
      "timeoutMs": {
       "type": "integer",
       "description": "How long to wait for the query to complete, in milliseconds, before returning. Default is to return immediately. If the timeout passes before the job completes, the request will fail with a TIMEOUT error",
       "format": "uint32",
       "location": "query"
      }
     },
     "parameterOrder": [
      "projectId",
      "jobId"
     ],
     "response": {
      "$ref": "GetQueryResultsResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "insert": {
     "id": "bigquery.jobs.insert",
     "path": "projects/{projectId}/jobs",
     "httpMethod": "POST",
     "description": "Starts a new asynchronous job.",
     "parameters": {
      "projectId": {
       "type": "string",
       "description": "Project ID of the project that will be billed for the job",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId"
     ],
     "request": {
      "$ref": "Job"
     },
     "response": {
      "$ref": "Job"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery",
      "https://www.googleapis.com/auth/devstorage.full_control",
      "https://www.googleapis.com/auth/devstorage.read_only",
      "https://www.googleapis.com/auth/devstorage.read_write"
     ],
     "supportsMediaUpload": true,
     "mediaUpload": {
      "accept": [
       "application/octet-stream"
      ],
      "protocols": {
       "simple": {
        "multipart": true,
        "path": "/upload/bigquery/v2/projects/{projectId}/jobs"
       },
       "resumable": {
        "multipart": true,
        "path": "/resumable/upload/bigquery/v2/projects/{projectId}/jobs"
       }
      }
     }
    },
    "list": {
     "id": "bigquery.jobs.list",
     "path": "projects/{projectId}/jobs",
     "httpMethod": "GET",
     "description": "Lists all the Jobs in the specified project that were started by the user.",
     "parameters": {
      "allUsers": {
       "type": "boolean",
       "description": "Whether to display jobs owned by all users in the project. Default false",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "format": "uint32",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Page token, returned by a previous call, to request the next page of results",
       "location": "query"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the jobs to list",
       "required": true,
       "location": "path"
      },
      "projection": {
       "type": "string",
       "description": "Restrict information returned to a set of selected fields",
       "enum": [
        "full",
        "minimal"
       ],
       "enumDescriptions": [
        "Includes all job data",
        "Does not include the job configuration"
       ],
       "location": "query"
      },
      "stateFilter": {
       "type": "string",
       "description": "Filter for job state",
       "enum": [
        "done",
        "pending",
        "running"
       ],
       "enumDescriptions": [
        "Finished jobs",
        "Pending jobs",
        "Running jobs"
       ],
       "repeated": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "projectId"
     ],
     "response": {
      "$ref": "JobList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "query": {
     "id": "bigquery.jobs.query",
     "path": "projects/{projectId}/queries",
     "httpMethod": "POST",
     "description": "Runs a BigQuery SQL query synchronously and returns query results if the query completes within a specified timeout.",
     "parameters": {
      "projectId": {
       "type": "string",
       "description": "Project ID of the project billed for the query",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId"
     ],
     "request": {
      "$ref": "QueryRequest"
     },
     "response": {
      "$ref": "QueryResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    }
   }
  },
  "projects": {
   "methods": {
    "list": {
     "id": "bigquery.projects.list",
     "path": "projects",
     "httpMethod": "GET",
     "description": "Lists the projects to which you have at least read access.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "format": "uint32",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Page token, returned by a previous call, to request the next page of results",
       "location": "query"
      }
     },
     "response": {
      "$ref": "ProjectList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    }
   }
  },
  "tabledata": {
   "methods": {
    "list": {
     "id": "bigquery.tabledata.list",
     "path": "projects/{projectId}/datasets/{datasetId}/tables/{tableId}/data",
     "httpMethod": "GET",
     "description": "Retrieves table data from a specified set of rows.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the table to read",
       "required": true,
       "location": "path"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "format": "uint32",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Page token, returned by a previous call, identifying the result set",
       "location": "query"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the table to read",
       "required": true,
       "location": "path"
      },
      "startIndex": {
       "type": "string",
       "description": "Zero-based index of the starting row to read",
       "format": "uint64",
       "location": "query"
      },
      "tableId": {
       "type": "string",
       "description": "Table ID of the table to read",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId",
      "tableId"
     ],
     "response": {
      "$ref": "TableDataList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    }
   }
  },
  "tables": {
   "methods": {
    "delete": {
     "id": "bigquery.tables.delete",
     "path": "projects/{projectId}/datasets/{datasetId}/tables/{tableId}",
     "httpMethod": "DELETE",
     "description": "Deletes the table specified by tableId from the dataset. If the table contains data, all the data will be deleted.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the table to delete",
       "required": true,
       "location": "path"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the table to delete",
       "required": true,
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table ID of the table to delete",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId",
      "tableId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "get": {
     "id": "bigquery.tables.get",
     "path": "projects/{projectId}/datasets/{datasetId}/tables/{tableId}",
     "httpMethod": "GET",
     "description": "Gets the specified table resource by table ID. This method does not return the data in the table, it only returns the table resource, which describes the structure of this table.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the requested table",
       "required": true,
       "location": "path"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the requested table",
       "required": true,
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table ID of the requested table",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId",
      "tableId"
     ],
     "response": {
      "$ref": "Table"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "insert": {
     "id": "bigquery.tables.insert",
     "path": "projects/{projectId}/datasets/{datasetId}/tables",
     "httpMethod": "POST",
     "description": "Creates a new, empty table in the dataset.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the new table",
       "required": true,
       "location": "path"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the new table",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId"
     ],
     "request": {
      "$ref": "Table"
     },
     "response": {
      "$ref": "Table"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "list": {
     "id": "bigquery.tables.list",
     "path": "projects/{projectId}/datasets/{datasetId}/tables",
     "httpMethod": "GET",
     "description": "Lists all tables in the specified dataset.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the tables to list",
       "required": true,
       "location": "path"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "format": "uint32",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Page token, returned by a previous call, to request the next page of results",
       "location": "query"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the tables to list",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId"
     ],
     "response": {
      "$ref": "TableList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "patch": {
     "id": "bigquery.tables.patch",
     "path": "projects/{projectId}/datasets/{datasetId}/tables/{tableId}",
     "httpMethod": "PATCH",
     "description": "Updates information in an existing table, specified by tableId. This method supports patch semantics.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the table to update",
       "required": true,
       "location": "path"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the table to update",
       "required": true,
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table ID of the table to update",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId",
      "tableId"
     ],
     "request": {
      "$ref": "Table"
     },
     "response": {
      "$ref": "Table"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    },
    "update": {
     "id": "bigquery.tables.update",
     "path": "projects/{projectId}/datasets/{datasetId}/tables/{tableId}",
     "httpMethod": "PUT",
     "description": "Updates information in an existing table, specified by tableId.",
     "parameters": {
      "datasetId": {
       "type": "string",
       "description": "Dataset ID of the table to update",
       "required": true,
       "location": "path"
      },
      "projectId": {
       "type": "string",
       "description": "Project ID of the table to update",
       "required": true,
       "location": "path"
      },
      "tableId": {
       "type": "string",
       "description": "Table ID of the table to update",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "projectId",
      "datasetId",
      "tableId"
     ],
     "request": {
      "$ref": "Table"
     },
     "response": {
      "$ref": "Table"
     },
     "scopes": [
      "https://www.googleapis.com/auth/bigquery"
     ]
    }
   }
  }
 }
}
