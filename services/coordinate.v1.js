{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "coordinate:v1",
 "name": "coordinate",
 "version": "v1",
 "revision": "20120912",
 "title": "Google Maps Coordinate API",
 "description": "Lets you view and manage jobs in a Coordinate team.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/search-16.gif",
  "x32": "http://www.google.com/images/icons/product/search-32.gif"
 },
 "documentationLink": "https://developers.google.com/coordinate/",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/coordinate/v1/teams/",
 "basePath": "/coordinate/v1/teams/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "coordinate/v1/teams/",
 "batchPath": "batch",
 "parameters": {
  "alt": {
   "type": "string",
   "description": "Data format for the response.",
   "default": "json",
   "enum": [
    "json"
   ],
   "enumDescriptions": [
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
    "https://www.googleapis.com/auth/coordinate": {
     "description": "View and manage your Google Maps Coordinate jobs"
    },
    "https://www.googleapis.com/auth/coordinate.readonly": {
     "description": "View your Google Coordinate jobs"
    }
   }
  }
 },
 "schemas": {
  "CustomField": {
   "id": "CustomField",
   "type": "object",
   "description": "Custom field.",
   "properties": {
    "customFieldId": {
     "type": "string",
     "description": "Custom field id.",
     "format": "int64"
    },
    "kind": {
     "type": "string",
     "description": "Identifies this object as a custom field.",
     "default": "coordinate#customField"
    },
    "value": {
     "type": "string",
     "description": "Custom field value."
    }
   }
  },
  "CustomFieldDef": {
   "id": "CustomFieldDef",
   "type": "object",
   "description": "Custom field definition.",
   "properties": {
    "enabled": {
     "type": "boolean",
     "description": "Whether the field is enabled."
    },
    "id": {
     "type": "string",
     "description": "Custom field id.",
     "format": "int64"
    },
    "kind": {
     "type": "string",
     "description": "Identifies this object as a custom field definition.",
     "default": "coordinate#customFieldDef"
    },
    "name": {
     "type": "string",
     "description": "Custom field name."
    },
    "requiredForCheckout": {
     "type": "boolean",
     "description": "Whether the field is required for checkout."
    },
    "type": {
     "type": "string",
     "description": "Custom field type."
    }
   }
  },
  "CustomFieldDefListResponse": {
   "id": "CustomFieldDefListResponse",
   "type": "object",
   "description": "Collection of custom field definitions for a team.",
   "properties": {
    "items": {
     "type": "array",
     "description": "Collection of custom field definitions in a team.",
     "items": {
      "$ref": "CustomFieldDef"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this object as a collection of custom field definitions in a team.",
     "default": "coordinate#customFieldDefList"
    }
   }
  },
  "CustomFields": {
   "id": "CustomFields",
   "type": "object",
   "description": "Collection of custom fields.",
   "properties": {
    "customField": {
     "type": "array",
     "description": "Collection of custom fields.",
     "items": {
      "$ref": "CustomField"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this object as a collection of custom fields.",
     "default": "coordinate#customFields"
    }
   }
  },
  "Job": {
   "id": "Job",
   "type": "object",
   "description": "A job.",
   "properties": {
    "id": {
     "type": "string",
     "description": "Job id.",
     "format": "uint64"
    },
    "jobChange": {
     "type": "array",
     "description": "List of job changes since it was created. The first change corresponds to the state of the job when it was created.",
     "items": {
      "$ref": "JobChange"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this object as a job.",
     "default": "coordinate#job"
    },
    "state": {
     "$ref": "JobState",
     "description": "Current job state."
    }
   }
  },
  "JobChange": {
   "id": "JobChange",
   "type": "object",
   "description": "Change to a job. For example assigning the job to a different worker.",
   "properties": {
    "kind": {
     "type": "string",
     "description": "Identifies this object as a job change.",
     "default": "coordinate#jobChange"
    },
    "state": {
     "$ref": "JobState",
     "description": "Change applied to the job. Only the fields that were changed are set."
    },
    "timestamp": {
     "type": "string",
     "description": "Time at which this change was applied.",
     "format": "uint64"
    }
   }
  },
  "JobListResponse": {
   "id": "JobListResponse",
   "type": "object",
   "description": "Response from a List Jobs request.",
   "properties": {
    "items": {
     "type": "array",
     "description": "Jobs in the collection.",
     "items": {
      "$ref": "Job"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this object as a list of jobs.",
     "default": "coordinate#jobList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token to provide to get the next page of results."
    }
   }
  },
  "JobState": {
   "id": "JobState",
   "type": "object",
   "description": "Current state of a job.",
   "properties": {
    "assignee": {
     "type": "string",
     "description": "Email address of the assignee."
    },
    "customFields": {
     "$ref": "CustomFields",
     "description": "Custom fields."
    },
    "customerName": {
     "type": "string",
     "description": "Customer name."
    },
    "customerPhoneNumber": {
     "type": "string",
     "description": "Customer phone number."
    },
    "kind": {
     "type": "string",
     "description": "Identifies this object as a job state.",
     "default": "coordinate#jobState"
    },
    "location": {
     "$ref": "Location",
     "description": "Job location."
    },
    "note": {
     "type": "array",
     "description": "Note added to the job.",
     "items": {
      "type": "string"
     }
    },
    "progress": {
     "type": "string",
     "description": "Job progress."
    },
    "title": {
     "type": "string",
     "description": "Job title."
    }
   }
  },
  "Location": {
   "id": "Location",
   "type": "object",
   "description": "Location of a job.",
   "properties": {
    "addressLine": {
     "type": "array",
     "description": "Address.",
     "items": {
      "type": "string"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this object as a location.",
     "default": "coordinate#location"
    },
    "lat": {
     "type": "number",
     "description": "Latitude.",
     "format": "double"
    },
    "lng": {
     "type": "number",
     "description": "Longitude.",
     "format": "double"
    }
   }
  }
 },
 "resources": {
  "customFieldDef": {
   "methods": {
    "list": {
     "id": "coordinate.customFieldDef.list",
     "path": "{teamId}/custom_fields",
     "httpMethod": "GET",
     "description": "Retrieves a list of custom field definitions for a team.",
     "parameters": {
      "teamId": {
       "type": "string",
       "description": "Team ID",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "teamId"
     ],
     "response": {
      "$ref": "CustomFieldDefListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/coordinate",
      "https://www.googleapis.com/auth/coordinate.readonly"
     ]
    }
   }
  },
  "jobs": {
   "methods": {
    "get": {
     "id": "coordinate.jobs.get",
     "path": "{teamId}/jobs/{jobId}",
     "httpMethod": "GET",
     "description": "Retrieves a job, including all the changes made to the job.",
     "parameters": {
      "jobId": {
       "type": "string",
       "description": "Job number",
       "required": true,
       "format": "uint64",
       "location": "path"
      },
      "teamId": {
       "type": "string",
       "description": "Team ID",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "teamId",
      "jobId"
     ],
     "response": {
      "$ref": "Job"
     },
     "scopes": [
      "https://www.googleapis.com/auth/coordinate",
      "https://www.googleapis.com/auth/coordinate.readonly"
     ]
    },
    "insert": {
     "id": "coordinate.jobs.insert",
     "path": "{teamId}/jobs",
     "httpMethod": "POST",
     "description": "Inserts a new job. Only the state field of the job should be set.",
     "parameters": {
      "address": {
       "type": "string",
       "description": "Job address as newline (Unix) separated string",
       "required": true,
       "location": "query"
      },
      "assignee": {
       "type": "string",
       "description": "Assignee email address, or empty string to unassign.",
       "location": "query"
      },
      "customField": {
       "type": "string",
       "description": "Map from custom field id (from /team//custom_fields) to the field value. For example '123=Alice'",
       "repeated": true,
       "location": "query"
      },
      "customerName": {
       "type": "string",
       "description": "Customer name",
       "location": "query"
      },
      "customerPhoneNumber": {
       "type": "string",
       "description": "Customer phone number",
       "location": "query"
      },
      "lat": {
       "type": "number",
       "description": "The latitude coordinate of this job's location.",
       "required": true,
       "format": "double",
       "location": "query"
      },
      "lng": {
       "type": "number",
       "description": "The longitude coordinate of this job's location.",
       "required": true,
       "format": "double",
       "location": "query"
      },
      "note": {
       "type": "string",
       "description": "Job note as newline (Unix) separated string",
       "location": "query"
      },
      "teamId": {
       "type": "string",
       "description": "Team ID",
       "required": true,
       "location": "path"
      },
      "title": {
       "type": "string",
       "description": "Job title",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "teamId",
      "address",
      "lat",
      "lng",
      "title"
     ],
     "request": {
      "$ref": "Job"
     },
     "response": {
      "$ref": "Job"
     },
     "scopes": [
      "https://www.googleapis.com/auth/coordinate"
     ]
    },
    "list": {
     "id": "coordinate.jobs.list",
     "path": "{teamId}/jobs",
     "httpMethod": "GET",
     "description": "Retrieves jobs created or modified since the given timestamp.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return in one page.",
       "format": "uint32",
       "location": "query"
      },
      "minModifiedTimestampMs": {
       "type": "string",
       "description": "Minimum time a job was modified in milliseconds since epoch.",
       "format": "uint64",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Continuation token",
       "location": "query"
      },
      "teamId": {
       "type": "string",
       "description": "Team ID",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "teamId"
     ],
     "response": {
      "$ref": "JobListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/coordinate",
      "https://www.googleapis.com/auth/coordinate.readonly"
     ]
    },
    "patch": {
     "id": "coordinate.jobs.patch",
     "path": "{teamId}/jobs/{jobId}",
     "httpMethod": "PATCH",
     "description": "Updates a job. Fields that are set in the job state will be updated. This method supports patch semantics.",
     "parameters": {
      "address": {
       "type": "string",
       "description": "Job address as newline (Unix) separated string",
       "location": "query"
      },
      "assignee": {
       "type": "string",
       "description": "Assignee email address, or empty string to unassign.",
       "location": "query"
      },
      "customField": {
       "type": "string",
       "description": "Map from custom field id (from /team//custom_fields) to the field value. For example '123=Alice'",
       "repeated": true,
       "location": "query"
      },
      "customerName": {
       "type": "string",
       "description": "Customer name",
       "location": "query"
      },
      "customerPhoneNumber": {
       "type": "string",
       "description": "Customer phone number",
       "location": "query"
      },
      "jobId": {
       "type": "string",
       "description": "Job number",
       "required": true,
       "format": "uint64",
       "location": "path"
      },
      "lat": {
       "type": "number",
       "description": "The latitude coordinate of this job's location.",
       "format": "double",
       "location": "query"
      },
      "lng": {
       "type": "number",
       "description": "The longitude coordinate of this job's location.",
       "format": "double",
       "location": "query"
      },
      "note": {
       "type": "string",
       "description": "Job note as newline (Unix) separated string",
       "location": "query"
      },
      "progress": {
       "type": "string",
       "description": "Job progress",
       "enum": [
        "COMPLETED",
        "IN_PROGRESS",
        "NOT_ACCEPTED",
        "NOT_STARTED",
        "OBSOLETE"
       ],
       "enumDescriptions": [
        "Completed",
        "In progress",
        "Not accepted",
        "Not started",
        "Obsolete"
       ],
       "location": "query"
      },
      "teamId": {
       "type": "string",
       "description": "Team ID",
       "required": true,
       "location": "path"
      },
      "title": {
       "type": "string",
       "description": "Job title",
       "location": "query"
      }
     },
     "parameterOrder": [
      "teamId",
      "jobId"
     ],
     "request": {
      "$ref": "Job"
     },
     "response": {
      "$ref": "Job"
     },
     "scopes": [
      "https://www.googleapis.com/auth/coordinate"
     ]
    },
    "update": {
     "id": "coordinate.jobs.update",
     "path": "{teamId}/jobs/{jobId}",
     "httpMethod": "PUT",
     "description": "Updates a job. Fields that are set in the job state will be updated.",
     "parameters": {
      "address": {
       "type": "string",
       "description": "Job address as newline (Unix) separated string",
       "location": "query"
      },
      "assignee": {
       "type": "string",
       "description": "Assignee email address, or empty string to unassign.",
       "location": "query"
      },
      "customField": {
       "type": "string",
       "description": "Map from custom field id (from /team//custom_fields) to the field value. For example '123=Alice'",
       "repeated": true,
       "location": "query"
      },
      "customerName": {
       "type": "string",
       "description": "Customer name",
       "location": "query"
      },
      "customerPhoneNumber": {
       "type": "string",
       "description": "Customer phone number",
       "location": "query"
      },
      "jobId": {
       "type": "string",
       "description": "Job number",
       "required": true,
       "format": "uint64",
       "location": "path"
      },
      "lat": {
       "type": "number",
       "description": "The latitude coordinate of this job's location.",
       "format": "double",
       "location": "query"
      },
      "lng": {
       "type": "number",
       "description": "The longitude coordinate of this job's location.",
       "format": "double",
       "location": "query"
      },
      "note": {
       "type": "string",
       "description": "Job note as newline (Unix) separated string",
       "location": "query"
      },
      "progress": {
       "type": "string",
       "description": "Job progress",
       "enum": [
        "COMPLETED",
        "IN_PROGRESS",
        "NOT_ACCEPTED",
        "NOT_STARTED",
        "OBSOLETE"
       ],
       "enumDescriptions": [
        "Completed",
        "In progress",
        "Not accepted",
        "Not started",
        "Obsolete"
       ],
       "location": "query"
      },
      "teamId": {
       "type": "string",
       "description": "Team ID",
       "required": true,
       "location": "path"
      },
      "title": {
       "type": "string",
       "description": "Job title",
       "location": "query"
      }
     },
     "parameterOrder": [
      "teamId",
      "jobId"
     ],
     "request": {
      "$ref": "Job"
     },
     "response": {
      "$ref": "Job"
     },
     "scopes": [
      "https://www.googleapis.com/auth/coordinate"
     ]
    }
   }
  }
 }
}
