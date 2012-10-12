{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "drive:v2",
 "name": "drive",
 "version": "v2",
 "revision": "20121008",
 "title": "Drive API",
 "description": "The API to interact with Drive.",
 "icons": {
  "x16": "https://ssl.gstatic.com/docs/doclist/images/drive_icon_16.png",
  "x32": "https://ssl.gstatic.com/docs/doclist/images/drive_icon_32.png"
 },
 "documentationLink": "https://developers.google.com/drive/",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/drive/v2/",
 "basePath": "/drive/v2/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "drive/v2/",
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
    "https://www.googleapis.com/auth/drive": {
     "description": "View and manage the files and documents in your Google Drive"
    },
    "https://www.googleapis.com/auth/drive.apps.readonly": {
     "description": "View your Google Drive apps"
    },
    "https://www.googleapis.com/auth/drive.file": {
     "description": "View and manage Google Drive files that you have opened or created with this app"
    },
    "https://www.googleapis.com/auth/drive.metadata.readonly": {
     "description": "View metadata for files and documents in your Google Drive"
    },
    "https://www.googleapis.com/auth/drive.readonly": {
     "description": "View the files and documents in your Google Drive"
    }
   }
  }
 },
 "schemas": {
  "About": {
   "id": "About",
   "type": "object",
   "description": "An item with user information and settings.",
   "properties": {
    "additionalRoleInfo": {
     "type": "array",
     "description": "Information about supported additional roles per file type. The most specific type takes precedence.",
     "items": {
      "type": "object",
      "properties": {
       "roleSets": {
        "type": "array",
        "description": "The supported additional roles per primary role.",
        "items": {
         "type": "object",
         "properties": {
          "additionalRoles": {
           "type": "array",
           "description": "The supported additional roles with the primary role.",
           "items": {
            "type": "string"
           }
          },
          "primaryRole": {
           "type": "string",
           "description": "A primary permission role."
          }
         }
        }
       },
       "type": {
        "type": "string",
        "description": "The content type that this additional role info applies to."
       }
      }
     }
    },
    "domainSharingPolicy": {
     "type": "string",
     "description": "The domain sharing policy for the current user."
    },
    "etag": {
     "type": "string",
     "description": "The ETag of the item."
    },
    "exportFormats": {
     "type": "array",
     "description": "The allowable export formats.",
     "items": {
      "type": "object",
      "properties": {
       "source": {
        "type": "string",
        "description": "The content type to convert from."
       },
       "targets": {
        "type": "array",
        "description": "The possible content types to convert to.",
        "items": {
         "type": "string"
        }
       }
      }
     }
    },
    "features": {
     "type": "array",
     "description": "List of additional features enabled on this account.",
     "items": {
      "type": "object",
      "properties": {
       "featureName": {
        "type": "string",
        "description": "The name of the feature."
       },
       "featureRate": {
        "type": "number",
        "description": "The request limit rate for this feature, in queries per second.",
        "format": "double"
       }
      }
     }
    },
    "importFormats": {
     "type": "array",
     "description": "The allowable import formats.",
     "items": {
      "type": "object",
      "properties": {
       "source": {
        "type": "string",
        "description": "The imported file's content type to convert from."
       },
       "targets": {
        "type": "array",
        "description": "The possible content types to convert to.",
        "items": {
         "type": "string"
        }
       }
      }
     }
    },
    "isCurrentAppInstalled": {
     "type": "boolean",
     "description": "A boolean indicating whether the authenticated app is installed by the authenticated user."
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#about.",
     "default": "drive#about"
    },
    "largestChangeId": {
     "type": "string",
     "description": "The largest change id.",
     "format": "int64"
    },
    "maxUploadSizes": {
     "type": "array",
     "description": "List of max upload sizes for each file type. The most specific type takes precedence.",
     "items": {
      "type": "object",
      "properties": {
       "size": {
        "type": "string",
        "description": "The max upload size for this type.",
        "format": "int64"
       },
       "type": {
        "type": "string",
        "description": "The file type."
       }
      }
     }
    },
    "name": {
     "type": "string",
     "description": "The name of the current user."
    },
    "permissionId": {
     "type": "string",
     "description": "The current user's ID as visible in the permissions collection."
    },
    "quotaBytesTotal": {
     "type": "string",
     "description": "The total number of quota bytes.",
     "format": "int64"
    },
    "quotaBytesUsed": {
     "type": "string",
     "description": "The number of quota bytes used.",
     "format": "int64"
    },
    "quotaBytesUsedAggregate": {
     "type": "string",
     "description": "The number of quota bytes used by all Google apps (Drive, Picasa, etc.).",
     "format": "int64"
    },
    "quotaBytesUsedInTrash": {
     "type": "string",
     "description": "The number of quota bytes used by trashed items.",
     "format": "int64"
    },
    "remainingChangeIds": {
     "type": "string",
     "description": "The number of remaining change ids.",
     "format": "int64"
    },
    "rootFolderId": {
     "type": "string",
     "description": "The id of the root folder."
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this item."
    },
    "user": {
     "$ref": "User",
     "description": "The authenticated user."
    }
   }
  },
  "App": {
   "id": "App",
   "type": "object",
   "description": "Information about a third-party application which the user has installed or given access to Google Drive.",
   "properties": {
    "authorized": {
     "type": "boolean",
     "description": "Whether the app is authorized to access data on the user's Drive."
    },
    "icons": {
     "type": "array",
     "description": "The various icons for the app.",
     "items": {
      "type": "object",
      "properties": {
       "category": {
        "type": "string",
        "description": "Category of the icon. Allowed values are:  \n- application - icon for the application \n- document - icon for a file associated with the app \n- documentShared - icon for a shared file associated with the app"
       },
       "iconUrl": {
        "type": "string",
        "description": "URL for the icon."
       },
       "size": {
        "type": "integer",
        "description": "Size of the icon. Represented as the maximum of the width and height.",
        "format": "int32"
       }
      }
     }
    },
    "id": {
     "type": "string",
     "description": "The ID of the app."
    },
    "installed": {
     "type": "boolean",
     "description": "Whether the app is installed."
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#app.",
     "default": "drive#app"
    },
    "name": {
     "type": "string",
     "description": "The name of the app."
    },
    "objectType": {
     "type": "string",
     "description": "The type of object this app creates (e.g. Chart). If empty, the app name should be used instead."
    },
    "primaryFileExtensions": {
     "type": "array",
     "description": "The list of primary file extensions.",
     "items": {
      "type": "string"
     }
    },
    "primaryMimeTypes": {
     "type": "array",
     "description": "The list of primary mime types.",
     "items": {
      "type": "string"
     }
    },
    "productUrl": {
     "type": "string",
     "description": "The product URL."
    },
    "secondaryFileExtensions": {
     "type": "array",
     "description": "The list of secondary file extensions.",
     "items": {
      "type": "string"
     }
    },
    "secondaryMimeTypes": {
     "type": "array",
     "description": "The list of secondary mime types.",
     "items": {
      "type": "string"
     }
    },
    "supportsCreate": {
     "type": "boolean",
     "description": "Whether this app supports creating new objects."
    },
    "supportsImport": {
     "type": "boolean",
     "description": "Whether this app supports importing Google Docs."
    },
    "useByDefault": {
     "type": "boolean",
     "description": "Whether the app is selected as the default handler for the types it supports."
    }
   }
  },
  "AppList": {
   "id": "AppList",
   "type": "object",
   "description": "A list of third-party applications which the user has installed or given access to Google Drive.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The ETag of the list."
    },
    "items": {
     "type": "array",
     "description": "The actual list of apps.",
     "items": {
      "$ref": "App"
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#appList.",
     "default": "drive#appList"
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this list."
    }
   }
  },
  "Change": {
   "id": "Change",
   "type": "object",
   "description": "Representation of a change to a file.",
   "properties": {
    "deleted": {
     "type": "boolean",
     "description": "Whether the file has been deleted."
    },
    "file": {
     "$ref": "File",
     "description": "The updated state of the file. Present if the file has not been deleted."
    },
    "fileId": {
     "type": "string",
     "description": "The ID of the file associated with this change."
    },
    "id": {
     "type": "string",
     "description": "The ID of the change.",
     "format": "int64"
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#change.",
     "default": "drive#change"
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this change."
    }
   }
  },
  "ChangeList": {
   "id": "ChangeList",
   "type": "object",
   "description": "A list of changes for a user.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The ETag of the list."
    },
    "items": {
     "type": "array",
     "description": "The actual list of changes.",
     "items": {
      "$ref": "Change"
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#changeList.",
     "default": "drive#changeList"
    },
    "largestChangeId": {
     "type": "string",
     "description": "The current largest change ID.",
     "format": "int64"
    },
    "nextLink": {
     "type": "string",
     "description": "A link to the next page of changes."
    },
    "nextPageToken": {
     "type": "string",
     "description": "The page token for the next page of changes."
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this list."
    }
   }
  },
  "ChildList": {
   "id": "ChildList",
   "type": "object",
   "description": "A list of children of a file.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The ETag of the list."
    },
    "items": {
     "type": "array",
     "description": "The actual list of children.",
     "items": {
      "$ref": "ChildReference"
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#childList.",
     "default": "drive#childList"
    },
    "nextLink": {
     "type": "string",
     "description": "A link to the next page of children."
    },
    "nextPageToken": {
     "type": "string",
     "description": "The page token for the next page of children."
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this list."
    }
   }
  },
  "ChildReference": {
   "id": "ChildReference",
   "type": "object",
   "description": "A reference to a file's child.",
   "properties": {
    "childLink": {
     "type": "string",
     "description": "A link to the child."
    },
    "id": {
     "type": "string",
     "description": "The ID of the child.",
     "annotations": {
      "required": [
       "drive.children.insert"
      ]
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#childReference.",
     "default": "drive#childReference"
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this reference."
    }
   }
  },
  "Comment": {
   "id": "Comment",
   "type": "object",
   "description": "A JSON representation of a comment on a file in Google Drive.",
   "properties": {
    "anchor": {
     "type": "string",
     "description": "A region of the document represented as a JSON string. See anchor documentation for details on how to define and interpret anchor properties."
    },
    "author": {
     "$ref": "User",
     "description": "The user who wrote this comment."
    },
    "commentId": {
     "type": "string",
     "description": "The ID of the comment."
    },
    "content": {
     "type": "string",
     "description": "The plain text content used to create this comment. This is not HTML safe and should only be used as a starting point to make edits to a comment's content.",
     "annotations": {
      "required": [
       "drive.comments.insert",
       "drive.comments.update"
      ]
     }
    },
    "context": {
     "type": "object",
     "description": "The context of the file which is being commented on.",
     "properties": {
      "type": {
       "type": "string",
       "description": "The MIME type of the context snippet."
      },
      "value": {
       "type": "string",
       "description": "Data representation of the segment of the file being commented on. In the case of a text file for example, this would be the actual text that the comment is about."
      }
     }
    },
    "createdDate": {
     "type": "string",
     "description": "The date when this comment was first created.",
     "format": "date-time"
    },
    "deleted": {
     "type": "boolean",
     "description": "Whether this comment has been deleted. If a comment has been deleted the content will be cleared and this will only represent a comment that once existed."
    },
    "fileId": {
     "type": "string",
     "description": "The file which this comment is addressing."
    },
    "fileTitle": {
     "type": "string",
     "description": "The title of the file which this comment is addressing."
    },
    "htmlContent": {
     "type": "string",
     "description": "HTML formatted content for this comment."
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#comment.",
     "default": "drive#comment"
    },
    "modifiedDate": {
     "type": "string",
     "description": "The date when this comment or any of its replies were last modified.",
     "format": "date-time"
    },
    "replies": {
     "type": "array",
     "description": "Replies to this post.",
     "items": {
      "$ref": "CommentReply"
     }
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this comment."
    },
    "status": {
     "type": "string",
     "description": "The status of this comment. Status can be changed by posting a reply to a comment with the desired status.  \n- \"open\" - The comment is still open. \n- \"resolved\" - The comment has been resolved by one of its replies."
    }
   }
  },
  "CommentList": {
   "id": "CommentList",
   "type": "object",
   "description": "A JSON representation of a list of comments on a file in Google Drive.",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of comments.",
     "items": {
      "$ref": "Comment"
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#commentList.",
     "default": "drive#commentList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "The token to use to request the next page of results."
    }
   }
  },
  "CommentReply": {
   "id": "CommentReply",
   "type": "object",
   "description": "A JSON representation of a reply to a comment on a file in Google Drive.",
   "properties": {
    "author": {
     "$ref": "User",
     "description": "The user who wrote this reply."
    },
    "content": {
     "type": "string",
     "description": "The plain text content used to create this reply. This is not HTML safe and should only be used as a starting point to make edits to a reply's content. This field is required on inserts if no verb is specified (resolve/reopen).",
     "annotations": {
      "required": [
       "drive.replies.update"
      ]
     }
    },
    "createdDate": {
     "type": "string",
     "description": "The date when this reply was first created.",
     "format": "date-time"
    },
    "deleted": {
     "type": "boolean",
     "description": "Whether this reply has been deleted. If a reply has been deleted the content will be cleared and this will only represent a reply that once existed."
    },
    "htmlContent": {
     "type": "string",
     "description": "HTML formatted content for this reply."
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#commentReply.",
     "default": "drive#commentReply"
    },
    "modifiedDate": {
     "type": "string",
     "description": "The date when this reply was last modified.",
     "format": "date-time"
    },
    "replyId": {
     "type": "string",
     "description": "The ID of the reply."
    },
    "verb": {
     "type": "string",
     "description": "The action this reply performed to the parent comment. When creating a new reply this is the action to be perform to the parent comment. Possible values are:  \n- \"resolve\" - To resolve a comment. \n- \"reopen\" - To reopen (un-resolve) a comment."
    }
   }
  },
  "CommentReplyList": {
   "id": "CommentReplyList",
   "type": "object",
   "description": "A JSON representation of a list of replies to a comment on a file in Google Drive.",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of reply.",
     "items": {
      "$ref": "CommentReply"
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#commentReplyList.",
     "default": "drive#commentReplyList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "The token to use to request the next page of results."
    }
   }
  },
  "File": {
   "id": "File",
   "type": "object",
   "description": "The metadata for a file.",
   "properties": {
    "alternateLink": {
     "type": "string",
     "description": "A link for opening the file in using a relevant Google editor or viewer."
    },
    "createdDate": {
     "type": "string",
     "description": "Create time for this file (formatted ISO8601 timestamp).",
     "format": "date-time"
    },
    "description": {
     "type": "string",
     "description": "A short description of the file."
    },
    "downloadUrl": {
     "type": "string",
     "description": "Short term download URL for the file. This will only be populated on files with content stored in Drive."
    },
    "editable": {
     "type": "boolean",
     "description": "Whether the file can be edited by the current user."
    },
    "embedLink": {
     "type": "string",
     "description": "A link for embedding the file."
    },
    "etag": {
     "type": "string",
     "description": "ETag of the file."
    },
    "explicitlyTrashed": {
     "type": "boolean",
     "description": "Whether this file has been explicitly trashed, as opposed to recursively trashed. This will only be populated if the file is trashed."
    },
    "exportLinks": {
     "type": "object",
     "description": "Links for exporting Google Docs to specific formats.",
     "additionalProperties": {
      "type": "string",
      "description": "A mapping from export format to URL"
     }
    },
    "fileExtension": {
     "type": "string",
     "description": "The file extension used when downloading this file. This field is set from the title when inserting or uploading new content. This will only be populated on files with content stored in Drive."
    },
    "fileSize": {
     "type": "string",
     "description": "The size of the file in bytes. This will only be populated on files with content stored in Drive.",
     "format": "int64"
    },
    "id": {
     "type": "string",
     "description": "The id of the file."
    },
    "imageMediaMetadata": {
     "type": "object",
     "description": "Metadata about image media. This will only be present for image types, and its contents will depend on what can be parsed from the image content.",
     "properties": {
      "aperture": {
       "type": "number",
       "description": "The aperture used to create the photo (f-number).",
       "format": "float"
      },
      "cameraMake": {
       "type": "string",
       "description": "The make of the camera used to create the photo."
      },
      "cameraModel": {
       "type": "string",
       "description": "The model of the camera used to create the photo."
      },
      "date": {
       "type": "string",
       "description": "The date and time the photo was taken (EXIF format timestamp)."
      },
      "exposureTime": {
       "type": "number",
       "description": "The length of the exposure, in seconds.",
       "format": "float"
      },
      "flashUsed": {
       "type": "boolean",
       "description": "Whether a flash was used to create the photo."
      },
      "focalLength": {
       "type": "number",
       "description": "The focal length used to create the photo, in millimeters.",
       "format": "float"
      },
      "height": {
       "type": "integer",
       "description": "The height of the image in pixels.",
       "format": "int32"
      },
      "isoSpeed": {
       "type": "integer",
       "description": "The ISO speed used to create the photo.",
       "format": "int32"
      },
      "location": {
       "type": "object",
       "description": "Geographic location information stored in the image.",
       "properties": {
        "altitude": {
         "type": "number",
         "description": "The altitude stored in the image.",
         "format": "double"
        },
        "latitude": {
         "type": "number",
         "description": "The latitude stored in the image.",
         "format": "double"
        },
        "longitude": {
         "type": "number",
         "description": "The longitude stored in the image.",
         "format": "double"
        }
       }
      },
      "rotation": {
       "type": "integer",
       "description": "The rotation in clockwise degrees from the image's original orientation.",
       "format": "int32"
      },
      "width": {
       "type": "integer",
       "description": "The width of the image in pixels.",
       "format": "int32"
      }
     }
    },
    "indexableText": {
     "type": "object",
     "description": "Indexable text attributes for the file (can only be written)",
     "properties": {
      "text": {
       "type": "string",
       "description": "The text to be indexed for this file"
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "The type of file. This is always drive#file.",
     "default": "drive#file"
    },
    "labels": {
     "type": "object",
     "description": "A group of labels for the file.",
     "properties": {
      "hidden": {
       "type": "boolean",
       "description": "Whether this file is hidden from the user."
      },
      "restricted": {
       "type": "boolean",
       "description": "Whether viewers are prevented from downloading this file."
      },
      "starred": {
       "type": "boolean",
       "description": "Whether this file is starred by the user."
      },
      "trashed": {
       "type": "boolean",
       "description": "Whether this file has been trashed."
      },
      "viewed": {
       "type": "boolean",
       "description": "Whether this file has been viewed by this user."
      }
     }
    },
    "lastModifyingUserName": {
     "type": "string",
     "description": "Name of the last user to modify this file. This will only be populated if a user has edited this file."
    },
    "lastViewedByMeDate": {
     "type": "string",
     "description": "Last time this file was viewed by the user (formatted RFC 3339 timestamp).",
     "format": "date-time"
    },
    "md5Checksum": {
     "type": "string",
     "description": "An MD5 checksum for the content of this file. This will only be populated on files with content stored in Drive."
    },
    "mimeType": {
     "type": "string",
     "description": "The MIME type of the file. This is only mutable on update when uploading new content. This field can be left blank, and the mimetype will be determined from the uploaded content's MIME type."
    },
    "modifiedByMeDate": {
     "type": "string",
     "description": "Last time this file was modified by the user (formatted RFC 3339 timestamp). Note that setting modifiedDate will also update the modifiedByMe date for the user which set the date.",
     "format": "date-time"
    },
    "modifiedDate": {
     "type": "string",
     "description": "Last time this file was modified by anyone (formatted RFC 3339 timestamp). This is only mutable on update when the setModifiedDate parameter is set.",
     "format": "date-time"
    },
    "originalFilename": {
     "type": "string",
     "description": "The original filename if the file was uploaded manually, or the original title if the file was inserted through the API. Note that renames of the title will not change the original filename. This will only be populated on files with content stored in Drive."
    },
    "ownerNames": {
     "type": "array",
     "description": "Name(s) of the owner(s) of this file.",
     "items": {
      "type": "string"
     }
    },
    "parents": {
     "type": "array",
     "description": "Collection of parent folders which contain this file.\nSetting this field will put the file in all of the provided folders. On insert, if no folders are provided, the file will be placed in the default root folder.",
     "items": {
      "$ref": "ParentReference"
     }
    },
    "quotaBytesUsed": {
     "type": "string",
     "description": "The number of quota bytes used by this file.",
     "format": "int64"
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this file."
    },
    "sharedWithMeDate": {
     "type": "string",
     "description": "Time at which this file was shared with the user (formatted RFC 3339 timestamp).",
     "format": "date-time"
    },
    "thumbnail": {
     "type": "object",
     "description": "Thumbnail for the file. Only accepted on upload and for files that are not already thumbnailed by Google.",
     "properties": {
      "image": {
       "type": "string",
       "description": "The URL-safe Base64 encoded bytes of the thumbnail image.",
       "format": "byte"
      },
      "mimeType": {
       "type": "string",
       "description": "The MIME type of the thumbnail."
      }
     }
    },
    "thumbnailLink": {
     "type": "string",
     "description": "A link to the file's thumbnail."
    },
    "title": {
     "type": "string",
     "description": "The title of this file."
    },
    "userPermission": {
     "$ref": "Permission",
     "description": "The permissions for the authenticated user on this file."
    },
    "webContentLink": {
     "type": "string",
     "description": "A link for downloading the content of the file in a browser using cookie based authentication. In cases where the content is shared publicly, the content can be downloaded without any credentials."
    },
    "writersCanShare": {
     "type": "boolean",
     "description": "Whether writers can share the document with other users."
    }
   }
  },
  "FileList": {
   "id": "FileList",
   "type": "object",
   "description": "A list of files.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The ETag of the list."
    },
    "items": {
     "type": "array",
     "description": "The actual list of files.",
     "items": {
      "$ref": "File"
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#fileList.",
     "default": "drive#fileList"
    },
    "nextLink": {
     "type": "string",
     "description": "A link to the next page of files."
    },
    "nextPageToken": {
     "type": "string",
     "description": "The page token for the next page of files."
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this list."
    }
   }
  },
  "ParentList": {
   "id": "ParentList",
   "type": "object",
   "description": "A list of a file's parents.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The ETag of the list."
    },
    "items": {
     "type": "array",
     "description": "The actual list of parents.",
     "items": {
      "$ref": "ParentReference"
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#parentList.",
     "default": "drive#parentList"
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this list."
    }
   }
  },
  "ParentReference": {
   "id": "ParentReference",
   "type": "object",
   "description": "A reference to a file's parent.",
   "properties": {
    "id": {
     "type": "string",
     "description": "The ID of the parent.",
     "annotations": {
      "required": [
       "drive.parents.insert"
      ]
     }
    },
    "isRoot": {
     "type": "boolean",
     "description": "Whether or not the parent is the root folder."
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#parentReference.",
     "default": "drive#parentReference"
    },
    "parentLink": {
     "type": "string",
     "description": "A link to the parent."
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this reference."
    }
   }
  },
  "Permission": {
   "id": "Permission",
   "type": "object",
   "description": "A single permission for a file.",
   "properties": {
    "additionalRoles": {
     "type": "array",
     "description": "Additional roles for this user. Only commenter is currently allowed.",
     "items": {
      "type": "string"
     }
    },
    "authKey": {
     "type": "string",
     "description": "The authkey parameter required for this permission."
    },
    "etag": {
     "type": "string",
     "description": "The ETag of the permission."
    },
    "id": {
     "type": "string",
     "description": "The ID of the permission."
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#permission.",
     "default": "drive#permission"
    },
    "name": {
     "type": "string",
     "description": "The name for this permission."
    },
    "photoLink": {
     "type": "string",
     "description": "A link to the profile photo, if available."
    },
    "role": {
     "type": "string",
     "description": "The primary role for this user. Allowed values are:  \n- owner \n- reader \n- writer",
     "annotations": {
      "required": [
       "drive.permissions.insert"
      ]
     }
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this permission."
    },
    "type": {
     "type": "string",
     "description": "The account type. Allowed values are:  \n- user \n- group \n- domain \n- anyone",
     "annotations": {
      "required": [
       "drive.permissions.insert"
      ]
     }
    },
    "value": {
     "type": "string",
     "description": "The email address or domain name for the entity. This is not populated in responses.",
     "annotations": {
      "required": [
       "drive.permissions.insert"
      ]
     }
    },
    "withLink": {
     "type": "boolean",
     "description": "Whether the link is required for this permission."
    }
   }
  },
  "PermissionList": {
   "id": "PermissionList",
   "type": "object",
   "description": "A list of permissions associated with a file.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The ETag of the list."
    },
    "items": {
     "type": "array",
     "description": "The actual list of permissions.",
     "items": {
      "$ref": "Permission"
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#permissionList.",
     "default": "drive#permissionList"
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this list."
    }
   }
  },
  "Revision": {
   "id": "Revision",
   "type": "object",
   "description": "A single revision of a file.",
   "properties": {
    "downloadUrl": {
     "type": "string",
     "description": "Short term download URL for the file. This will only be populated on files with content stored in Drive."
    },
    "etag": {
     "type": "string",
     "description": "The ETag of the revision."
    },
    "exportLinks": {
     "type": "object",
     "description": "Links for exporting Google Docs to specific formats.",
     "additionalProperties": {
      "type": "string",
      "description": "A mapping from export format to URL"
     }
    },
    "fileSize": {
     "type": "string",
     "description": "The size of the revision in bytes. This will only be populated on files with content stored in Drive.",
     "format": "int64"
    },
    "id": {
     "type": "string",
     "description": "The ID of the revision."
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#revision.",
     "default": "drive#revision"
    },
    "lastModifyingUserName": {
     "type": "string",
     "description": "Name of the last user to modify this revision."
    },
    "md5Checksum": {
     "type": "string",
     "description": "An MD5 checksum for the content of this revision. This will only be populated on files with content stored in Drive."
    },
    "mimeType": {
     "type": "string",
     "description": "The MIME type of the revision."
    },
    "modifiedDate": {
     "type": "string",
     "description": "Last time this revision was modified (formatted RFC 3339 timestamp).",
     "format": "date-time"
    },
    "originalFilename": {
     "type": "string",
     "description": "The original filename when this revision was created. This will only be populated on files with content stored in Drive."
    },
    "pinned": {
     "type": "boolean",
     "description": "Whether this revision is pinned to prevent automatic purging. This will only be populated and can only be modified on files with content stored in Drive which are not Google Docs. Revisions can also be pinned when they are created through the drive.files.insert/update/copy by using the pinned query parameter."
    },
    "publishAuto": {
     "type": "boolean",
     "description": "Whether subsequent revisions will be automatically republished. This is only populated and can only be modified for Google Docs."
    },
    "published": {
     "type": "boolean",
     "description": "Whether this revision is published. This is only populated and can only be modified for Google Docs."
    },
    "publishedLink": {
     "type": "string",
     "description": "A link to the published revision."
    },
    "publishedOutsideDomain": {
     "type": "boolean",
     "description": "Whether this revision is published outside the domain. This is only populated and can only be modified for Google Docs."
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this revision."
    }
   }
  },
  "RevisionList": {
   "id": "RevisionList",
   "type": "object",
   "description": "A list of revisions of a file.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The ETag of the list."
    },
    "items": {
     "type": "array",
     "description": "The actual list of revisions.",
     "items": {
      "$ref": "Revision"
     }
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#revisionList.",
     "default": "drive#revisionList"
    },
    "selfLink": {
     "type": "string",
     "description": "A link back to this list."
    }
   }
  },
  "User": {
   "id": "User",
   "type": "object",
   "description": "The JSON template for a user.",
   "properties": {
    "displayName": {
     "type": "string",
     "description": "A plain text displayable name for this user."
    },
    "isAuthenticatedUser": {
     "type": "boolean",
     "description": "Whether this user is the same as the authenticated user of which the request was made on behalf."
    },
    "kind": {
     "type": "string",
     "description": "This is always drive#user.",
     "default": "drive#user"
    },
    "picture": {
     "type": "object",
     "description": "The user's profile picture.",
     "properties": {
      "url": {
       "type": "string",
       "description": "A URL that points to a profile picture of this user."
      }
     }
    }
   }
  }
 },
 "resources": {
  "about": {
   "methods": {
    "get": {
     "id": "drive.about.get",
     "path": "about",
     "httpMethod": "GET",
     "description": "Gets the information about the current user along with Drive API settings",
     "parameters": {
      "includeSubscribed": {
       "type": "boolean",
       "description": "Whether to include subscribed items when calculating the number of remaining change IDs",
       "default": "true",
       "location": "query"
      },
      "maxChangeIdCount": {
       "type": "string",
       "description": "Maximum number of remaining change IDs to count",
       "default": "1",
       "format": "int64",
       "location": "query"
      },
      "startChangeId": {
       "type": "string",
       "description": "Change ID to start counting from when calculating number of remaining change IDs",
       "format": "int64",
       "location": "query"
      }
     },
     "response": {
      "$ref": "About"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    }
   }
  },
  "apps": {
   "methods": {
    "get": {
     "id": "drive.apps.get",
     "path": "apps/{appId}",
     "httpMethod": "GET",
     "description": "Gets a specific app.",
     "parameters": {
      "appId": {
       "type": "string",
       "description": "The ID of the app.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "appId"
     ],
     "response": {
      "$ref": "App"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive.apps.readonly"
     ]
    },
    "list": {
     "id": "drive.apps.list",
     "path": "apps",
     "httpMethod": "GET",
     "description": "Lists a user's apps.",
     "response": {
      "$ref": "AppList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive.apps.readonly"
     ]
    }
   }
  },
  "changes": {
   "methods": {
    "get": {
     "id": "drive.changes.get",
     "path": "changes/{changeId}",
     "httpMethod": "GET",
     "description": "Gets a specific change.",
     "parameters": {
      "changeId": {
       "type": "string",
       "description": "The ID of the change.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "changeId"
     ],
     "response": {
      "$ref": "Change"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "list": {
     "id": "drive.changes.list",
     "path": "changes",
     "httpMethod": "GET",
     "description": "Lists the changes for a user.",
     "parameters": {
      "includeDeleted": {
       "type": "boolean",
       "description": "Whether to include deleted items.",
       "default": "true",
       "location": "query"
      },
      "includeSubscribed": {
       "type": "boolean",
       "description": "Whether to include subscribed items.",
       "default": "true",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of changes to return.",
       "default": "100",
       "format": "int32",
       "minimum": "0",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Page token for changes.",
       "location": "query"
      },
      "startChangeId": {
       "type": "string",
       "description": "Change ID to start listing changes from.",
       "format": "int64",
       "location": "query"
      }
     },
     "response": {
      "$ref": "ChangeList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    }
   }
  },
  "children": {
   "methods": {
    "delete": {
     "id": "drive.children.delete",
     "path": "files/{folderId}/children/{childId}",
     "httpMethod": "DELETE",
     "description": "Removes a child from a folder.",
     "parameters": {
      "childId": {
       "type": "string",
       "description": "The ID of the child.",
       "required": true,
       "location": "path"
      },
      "folderId": {
       "type": "string",
       "description": "The ID of the folder.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "folderId",
      "childId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "get": {
     "id": "drive.children.get",
     "path": "files/{folderId}/children/{childId}",
     "httpMethod": "GET",
     "description": "Gets a specific child reference.",
     "parameters": {
      "childId": {
       "type": "string",
       "description": "The ID of the child.",
       "required": true,
       "location": "path"
      },
      "folderId": {
       "type": "string",
       "description": "The ID of the folder.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "folderId",
      "childId"
     ],
     "response": {
      "$ref": "ChildReference"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "insert": {
     "id": "drive.children.insert",
     "path": "files/{folderId}/children",
     "httpMethod": "POST",
     "description": "Inserts a file into a folder.",
     "parameters": {
      "folderId": {
       "type": "string",
       "description": "The ID of the folder.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "folderId"
     ],
     "request": {
      "$ref": "ChildReference"
     },
     "response": {
      "$ref": "ChildReference"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "list": {
     "id": "drive.children.list",
     "path": "files/{folderId}/children",
     "httpMethod": "GET",
     "description": "Lists a folder's children.",
     "parameters": {
      "folderId": {
       "type": "string",
       "description": "The ID of the folder.",
       "required": true,
       "location": "path"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of children to return.",
       "default": "100",
       "format": "int32",
       "minimum": "0",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Page token for children.",
       "location": "query"
      },
      "q": {
       "type": "string",
       "description": "Query string for searching children.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "folderId"
     ],
     "response": {
      "$ref": "ChildList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    }
   }
  },
  "comments": {
   "methods": {
    "delete": {
     "id": "drive.comments.delete",
     "path": "files/{fileId}/comments/{commentId}",
     "httpMethod": "DELETE",
     "description": "Deletes a comment.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "get": {
     "id": "drive.comments.get",
     "path": "files/{fileId}/comments/{commentId}",
     "httpMethod": "GET",
     "description": "Gets a comment by ID.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId"
     ],
     "response": {
      "$ref": "Comment"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "insert": {
     "id": "drive.comments.insert",
     "path": "files/{fileId}/comments",
     "httpMethod": "POST",
     "description": "Creates a new comment on the given file.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "request": {
      "$ref": "Comment"
     },
     "response": {
      "$ref": "Comment"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "list": {
     "id": "drive.comments.list",
     "path": "files/{fileId}/comments",
     "httpMethod": "GET",
     "description": "Lists a file's comments.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "includeDeleted": {
       "type": "boolean",
       "description": "If set, all comments, including deleted comments (with content stripped) will be returned.",
       "default": "false",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of discussions to include in the response, used for paging.",
       "default": "20",
       "format": "int32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The continuation token, used to page through large result sets. To get the next page of results, set this parameter to the value of \"nextPageToken\" from the previous response.",
       "location": "query"
      },
      "updatedMin": {
       "type": "string",
       "description": "Only discussions that were updated after this timestamp will be returned. Formatted as an RFC 3339 timestamp.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "response": {
      "$ref": "CommentList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "patch": {
     "id": "drive.comments.patch",
     "path": "files/{fileId}/comments/{commentId}",
     "httpMethod": "PATCH",
     "description": "Updates an existing comment. This method supports patch semantics.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId"
     ],
     "request": {
      "$ref": "Comment"
     },
     "response": {
      "$ref": "Comment"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive"
     ]
    },
    "update": {
     "id": "drive.comments.update",
     "path": "files/{fileId}/comments/{commentId}",
     "httpMethod": "PUT",
     "description": "Updates an existing comment.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId"
     ],
     "request": {
      "$ref": "Comment"
     },
     "response": {
      "$ref": "Comment"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive"
     ]
    }
   }
  },
  "files": {
   "methods": {
    "copy": {
     "id": "drive.files.copy",
     "path": "files/{fileId}/copy",
     "httpMethod": "POST",
     "description": "Creates a copy of the specified file.",
     "parameters": {
      "convert": {
       "type": "boolean",
       "description": "Whether to convert this file to the corresponding Google Docs format.",
       "default": "false",
       "location": "query"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file to copy.",
       "required": true,
       "location": "path"
      },
      "ocr": {
       "type": "boolean",
       "description": "Whether to attempt OCR on .jpg, .png, .gif, or .pdf uploads.",
       "default": "false",
       "location": "query"
      },
      "ocrLanguage": {
       "type": "string",
       "description": "If ocr is true, hints at the language to use. Valid values are ISO 639-1 codes.",
       "location": "query"
      },
      "pinned": {
       "type": "boolean",
       "description": "Whether to pin the head revision of the new copy.",
       "default": "false",
       "location": "query"
      },
      "sourceLanguage": {
       "type": "string",
       "description": "The language of the original file to be translated.",
       "location": "query"
      },
      "targetLanguage": {
       "type": "string",
       "description": "Target language to translate the file to. If no sourceLanguage is provided, the API will attempt to detect the language.",
       "location": "query"
      },
      "timedTextLanguage": {
       "type": "string",
       "description": "The language of the timed text.",
       "location": "query"
      },
      "timedTextTrackName": {
       "type": "string",
       "description": "The timed text track name.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "request": {
      "$ref": "File"
     },
     "response": {
      "$ref": "File"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "delete": {
     "id": "drive.files.delete",
     "path": "files/{fileId}",
     "httpMethod": "DELETE",
     "description": "Permanently deletes a file by ID. Skips the trash.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file to delete.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "get": {
     "id": "drive.files.get",
     "path": "files/{fileId}",
     "httpMethod": "GET",
     "description": "Gets a file's metadata by ID.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID for the file in question.",
       "required": true,
       "location": "path"
      },
      "projection": {
       "type": "string",
       "description": "This parameter is deprecated and has no function.",
       "enum": [
        "BASIC",
        "FULL"
       ],
       "enumDescriptions": [
        "Deprecated",
        "Deprecated"
       ],
       "location": "query"
      },
      "updateViewedDate": {
       "type": "boolean",
       "description": "Whether to update the view date after successfully retrieving the file.",
       "default": "false",
       "location": "query"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "response": {
      "$ref": "File"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "insert": {
     "id": "drive.files.insert",
     "path": "files",
     "httpMethod": "POST",
     "description": "Insert a new file.",
     "parameters": {
      "convert": {
       "type": "boolean",
       "description": "Whether to convert this file to the corresponding Google Docs format.",
       "default": "false",
       "location": "query"
      },
      "ocr": {
       "type": "boolean",
       "description": "Whether to attempt OCR on .jpg, .png, .gif, or .pdf uploads.",
       "default": "false",
       "location": "query"
      },
      "ocrLanguage": {
       "type": "string",
       "description": "If ocr is true, hints at the language to use. Valid values are ISO 639-1 codes.",
       "location": "query"
      },
      "pinned": {
       "type": "boolean",
       "description": "Whether to pin the head revision of the uploaded file.",
       "default": "false",
       "location": "query"
      },
      "sourceLanguage": {
       "type": "string",
       "description": "The language of the original file to be translated.",
       "location": "query"
      },
      "targetLanguage": {
       "type": "string",
       "description": "Target language to translate the file to. If no sourceLanguage is provided, the API will attempt to detect the language.",
       "location": "query"
      },
      "timedTextLanguage": {
       "type": "string",
       "description": "The language of the timed text.",
       "location": "query"
      },
      "timedTextTrackName": {
       "type": "string",
       "description": "The timed text track name.",
       "location": "query"
      }
     },
     "request": {
      "$ref": "File"
     },
     "response": {
      "$ref": "File"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ],
     "supportsMediaUpload": true,
     "mediaUpload": {
      "accept": [
       "*/*"
      ],
      "maxSize": "10GB",
      "protocols": {
       "simple": {
        "multipart": true,
        "path": "/upload/drive/v2/files"
       },
       "resumable": {
        "multipart": true,
        "path": "/resumable/upload/drive/v2/files"
       }
      }
     }
    },
    "list": {
     "id": "drive.files.list",
     "path": "files",
     "httpMethod": "GET",
     "description": "Lists the user's files.",
     "parameters": {
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of files to return.",
       "default": "100",
       "format": "int32",
       "minimum": "0",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Page token for files.",
       "location": "query"
      },
      "projection": {
       "type": "string",
       "description": "This parameter is deprecated and has no function.",
       "enum": [
        "BASIC",
        "FULL"
       ],
       "enumDescriptions": [
        "Deprecated",
        "Deprecated"
       ],
       "location": "query"
      },
      "q": {
       "type": "string",
       "description": "Query string for searching files.",
       "location": "query"
      }
     },
     "response": {
      "$ref": "FileList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "patch": {
     "id": "drive.files.patch",
     "path": "files/{fileId}",
     "httpMethod": "PATCH",
     "description": "Updates file metadata and/or content. This method supports patch semantics.",
     "parameters": {
      "convert": {
       "type": "boolean",
       "description": "Whether to convert this file to the corresponding Google Docs format.",
       "default": "false",
       "location": "query"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file to update.",
       "required": true,
       "location": "path"
      },
      "newRevision": {
       "type": "boolean",
       "description": "Whether a blob upload should create a new revision. If false, the blob data in the current head revision will be replaced.",
       "default": "true",
       "location": "query"
      },
      "ocr": {
       "type": "boolean",
       "description": "Whether to attempt OCR on .jpg, .png, .gif, or .pdf uploads.",
       "default": "false",
       "location": "query"
      },
      "ocrLanguage": {
       "type": "string",
       "description": "If ocr is true, hints at the language to use. Valid values are ISO 639-1 codes.",
       "location": "query"
      },
      "pinned": {
       "type": "boolean",
       "description": "Whether to pin the new revision.",
       "default": "false",
       "location": "query"
      },
      "setModifiedDate": {
       "type": "boolean",
       "description": "Whether to set the modified date with the supplied modified date.",
       "default": "false",
       "location": "query"
      },
      "sourceLanguage": {
       "type": "string",
       "description": "The language of the original file to be translated.",
       "location": "query"
      },
      "targetLanguage": {
       "type": "string",
       "description": "Target language to translate the file to. If no sourceLanguage is provided, the API will attempt to detect the language.",
       "location": "query"
      },
      "timedTextLanguage": {
       "type": "string",
       "description": "The language of the timed text.",
       "location": "query"
      },
      "timedTextTrackName": {
       "type": "string",
       "description": "The timed text track name.",
       "location": "query"
      },
      "updateViewedDate": {
       "type": "boolean",
       "description": "Whether to update the view date after successfully updating the file.",
       "default": "true",
       "location": "query"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "request": {
      "$ref": "File"
     },
     "response": {
      "$ref": "File"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "touch": {
     "id": "drive.files.touch",
     "path": "files/{fileId}/touch",
     "httpMethod": "POST",
     "description": "Set the file's updated time to the current server time.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file to update.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "response": {
      "$ref": "File"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "trash": {
     "id": "drive.files.trash",
     "path": "files/{fileId}/trash",
     "httpMethod": "POST",
     "description": "Moves a file to the trash.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file to trash.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "response": {
      "$ref": "File"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "untrash": {
     "id": "drive.files.untrash",
     "path": "files/{fileId}/untrash",
     "httpMethod": "POST",
     "description": "Restores a file from the trash.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file to untrash.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "response": {
      "$ref": "File"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "update": {
     "id": "drive.files.update",
     "path": "files/{fileId}",
     "httpMethod": "PUT",
     "description": "Updates file metadata and/or content",
     "parameters": {
      "convert": {
       "type": "boolean",
       "description": "Whether to convert this file to the corresponding Google Docs format.",
       "default": "false",
       "location": "query"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file to update.",
       "required": true,
       "location": "path"
      },
      "newRevision": {
       "type": "boolean",
       "description": "Whether a blob upload should create a new revision. If false, the blob data in the current head revision will be replaced.",
       "default": "true",
       "location": "query"
      },
      "ocr": {
       "type": "boolean",
       "description": "Whether to attempt OCR on .jpg, .png, .gif, or .pdf uploads.",
       "default": "false",
       "location": "query"
      },
      "ocrLanguage": {
       "type": "string",
       "description": "If ocr is true, hints at the language to use. Valid values are ISO 639-1 codes.",
       "location": "query"
      },
      "pinned": {
       "type": "boolean",
       "description": "Whether to pin the new revision.",
       "default": "false",
       "location": "query"
      },
      "setModifiedDate": {
       "type": "boolean",
       "description": "Whether to set the modified date with the supplied modified date.",
       "default": "false",
       "location": "query"
      },
      "sourceLanguage": {
       "type": "string",
       "description": "The language of the original file to be translated.",
       "location": "query"
      },
      "targetLanguage": {
       "type": "string",
       "description": "Target language to translate the file to. If no sourceLanguage is provided, the API will attempt to detect the language.",
       "location": "query"
      },
      "timedTextLanguage": {
       "type": "string",
       "description": "The language of the timed text.",
       "location": "query"
      },
      "timedTextTrackName": {
       "type": "string",
       "description": "The timed text track name.",
       "location": "query"
      },
      "updateViewedDate": {
       "type": "boolean",
       "description": "Whether to update the view date after successfully updating the file.",
       "default": "true",
       "location": "query"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "request": {
      "$ref": "File"
     },
     "response": {
      "$ref": "File"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ],
     "supportsMediaUpload": true,
     "mediaUpload": {
      "accept": [
       "*/*"
      ],
      "maxSize": "10GB",
      "protocols": {
       "simple": {
        "multipart": true,
        "path": "/upload/drive/v2/files/{fileId}"
       },
       "resumable": {
        "multipart": true,
        "path": "/resumable/upload/drive/v2/files/{fileId}"
       }
      }
     }
    }
   }
  },
  "parents": {
   "methods": {
    "delete": {
     "id": "drive.parents.delete",
     "path": "files/{fileId}/parents/{parentId}",
     "httpMethod": "DELETE",
     "description": "Removes a parent from a file.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "parentId": {
       "type": "string",
       "description": "The ID of the parent.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "parentId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "get": {
     "id": "drive.parents.get",
     "path": "files/{fileId}/parents/{parentId}",
     "httpMethod": "GET",
     "description": "Gets a specific parent reference.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "parentId": {
       "type": "string",
       "description": "The ID of the parent.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "parentId"
     ],
     "response": {
      "$ref": "ParentReference"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "insert": {
     "id": "drive.parents.insert",
     "path": "files/{fileId}/parents",
     "httpMethod": "POST",
     "description": "Adds a parent folder for a file.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "request": {
      "$ref": "ParentReference"
     },
     "response": {
      "$ref": "ParentReference"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "list": {
     "id": "drive.parents.list",
     "path": "files/{fileId}/parents",
     "httpMethod": "GET",
     "description": "Lists a file's parents.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "response": {
      "$ref": "ParentList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    }
   }
  },
  "permissions": {
   "methods": {
    "delete": {
     "id": "drive.permissions.delete",
     "path": "files/{fileId}/permissions/{permissionId}",
     "httpMethod": "DELETE",
     "description": "Deletes a permission from a file.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID for the file.",
       "required": true,
       "location": "path"
      },
      "permissionId": {
       "type": "string",
       "description": "The ID for the permission.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "permissionId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "get": {
     "id": "drive.permissions.get",
     "path": "files/{fileId}/permissions/{permissionId}",
     "httpMethod": "GET",
     "description": "Gets a permission by ID.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID for the file.",
       "required": true,
       "location": "path"
      },
      "permissionId": {
       "type": "string",
       "description": "The ID for the permission.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "permissionId"
     ],
     "response": {
      "$ref": "Permission"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "insert": {
     "id": "drive.permissions.insert",
     "path": "files/{fileId}/permissions",
     "httpMethod": "POST",
     "description": "Inserts a permission for a file.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID for the file.",
       "required": true,
       "location": "path"
      },
      "sendNotificationEmails": {
       "type": "boolean",
       "description": "Whether to send notification emails.",
       "default": "true",
       "location": "query"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "request": {
      "$ref": "Permission"
     },
     "response": {
      "$ref": "Permission"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "list": {
     "id": "drive.permissions.list",
     "path": "files/{fileId}/permissions",
     "httpMethod": "GET",
     "description": "Lists a file's permissions.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID for the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "response": {
      "$ref": "PermissionList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "patch": {
     "id": "drive.permissions.patch",
     "path": "files/{fileId}/permissions/{permissionId}",
     "httpMethod": "PATCH",
     "description": "Updates a permission. This method supports patch semantics.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID for the file.",
       "required": true,
       "location": "path"
      },
      "permissionId": {
       "type": "string",
       "description": "The ID for the permission.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "permissionId"
     ],
     "request": {
      "$ref": "Permission"
     },
     "response": {
      "$ref": "Permission"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "update": {
     "id": "drive.permissions.update",
     "path": "files/{fileId}/permissions/{permissionId}",
     "httpMethod": "PUT",
     "description": "Updates a permission.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID for the file.",
       "required": true,
       "location": "path"
      },
      "permissionId": {
       "type": "string",
       "description": "The ID for the permission.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "permissionId"
     ],
     "request": {
      "$ref": "Permission"
     },
     "response": {
      "$ref": "Permission"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    }
   }
  },
  "replies": {
   "methods": {
    "delete": {
     "id": "drive.replies.delete",
     "path": "files/{fileId}/comments/{commentId}/replies/{replyId}",
     "httpMethod": "DELETE",
     "description": "Deletes a reply.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "replyId": {
       "type": "string",
       "description": "The ID of the reply.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId",
      "replyId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/drive"
     ]
    },
    "get": {
     "id": "drive.replies.get",
     "path": "files/{fileId}/comments/{commentId}/replies/{replyId}",
     "httpMethod": "GET",
     "description": "Gets a reply.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "replyId": {
       "type": "string",
       "description": "The ID of the reply.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId",
      "replyId"
     ],
     "response": {
      "$ref": "CommentReply"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "insert": {
     "id": "drive.replies.insert",
     "path": "files/{fileId}/comments/{commentId}/replies",
     "httpMethod": "POST",
     "description": "Creates a new reply to the given comment.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId"
     ],
     "request": {
      "$ref": "CommentReply"
     },
     "response": {
      "$ref": "CommentReply"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive"
     ]
    },
    "list": {
     "id": "drive.replies.list",
     "path": "files/{fileId}/comments/{commentId}/replies",
     "httpMethod": "GET",
     "description": "Lists all of the replies to a comment.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of replies to include in the response, used for paging.",
       "default": "20",
       "format": "int32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "The continuation token, used to page through large result sets. To get the next page of results, set this parameter to the value of \"nextPageToken\" from the previous response.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId"
     ],
     "response": {
      "$ref": "CommentReplyList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "patch": {
     "id": "drive.replies.patch",
     "path": "files/{fileId}/comments/{commentId}/replies/{replyId}",
     "httpMethod": "PATCH",
     "description": "Updates an existing reply. This method supports patch semantics.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "replyId": {
       "type": "string",
       "description": "The ID of the reply.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId",
      "replyId"
     ],
     "request": {
      "$ref": "CommentReply"
     },
     "response": {
      "$ref": "CommentReply"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive"
     ]
    },
    "update": {
     "id": "drive.replies.update",
     "path": "files/{fileId}/comments/{commentId}/replies/{replyId}",
     "httpMethod": "PUT",
     "description": "Updates an existing reply.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "The ID of the comment.",
       "required": true,
       "location": "path"
      },
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "replyId": {
       "type": "string",
       "description": "The ID of the reply.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "commentId",
      "replyId"
     ],
     "request": {
      "$ref": "CommentReply"
     },
     "response": {
      "$ref": "CommentReply"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive"
     ]
    }
   }
  },
  "revisions": {
   "methods": {
    "delete": {
     "id": "drive.revisions.delete",
     "path": "files/{fileId}/revisions/{revisionId}",
     "httpMethod": "DELETE",
     "description": "Removes a revision.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "revisionId": {
       "type": "string",
       "description": "The ID of the revision.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "revisionId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "get": {
     "id": "drive.revisions.get",
     "path": "files/{fileId}/revisions/{revisionId}",
     "httpMethod": "GET",
     "description": "Gets a specific revision.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      },
      "revisionId": {
       "type": "string",
       "description": "The ID of the revision.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "revisionId"
     ],
     "response": {
      "$ref": "Revision"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "list": {
     "id": "drive.revisions.list",
     "path": "files/{fileId}/revisions",
     "httpMethod": "GET",
     "description": "Lists a file's revisions.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID of the file.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId"
     ],
     "response": {
      "$ref": "RevisionList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file",
      "https://www.googleapis.com/auth/drive.metadata.readonly",
      "https://www.googleapis.com/auth/drive.readonly"
     ]
    },
    "patch": {
     "id": "drive.revisions.patch",
     "path": "files/{fileId}/revisions/{revisionId}",
     "httpMethod": "PATCH",
     "description": "Updates a revision. This method supports patch semantics.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID for the file.",
       "required": true,
       "location": "path"
      },
      "revisionId": {
       "type": "string",
       "description": "The ID for the revision.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "revisionId"
     ],
     "request": {
      "$ref": "Revision"
     },
     "response": {
      "$ref": "Revision"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    },
    "update": {
     "id": "drive.revisions.update",
     "path": "files/{fileId}/revisions/{revisionId}",
     "httpMethod": "PUT",
     "description": "Updates a revision.",
     "parameters": {
      "fileId": {
       "type": "string",
       "description": "The ID for the file.",
       "required": true,
       "location": "path"
      },
      "revisionId": {
       "type": "string",
       "description": "The ID for the revision.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "fileId",
      "revisionId"
     ],
     "request": {
      "$ref": "Revision"
     },
     "response": {
      "$ref": "Revision"
     },
     "scopes": [
      "https://www.googleapis.com/auth/drive",
      "https://www.googleapis.com/auth/drive.file"
     ]
    }
   }
  }
 }
}
