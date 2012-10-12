{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "groupssettings:v1",
 "name": "groupssettings",
 "version": "v1",
 "revision": "20120831",
 "title": "Groups Settings API",
 "description": "Lets you manage permission levels and related settings of a group.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/search-16.gif",
  "x32": "http://www.google.com/images/icons/product/search-32.gif"
 },
 "documentationLink": "https://developers.google.com/google-apps/groups-settings/get_started",
 "labels": [
  "limited_availability"
 ],
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/groups/v1/groups/",
 "basePath": "/groups/v1/groups/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "groups/v1/groups/",
 "batchPath": "batch",
 "parameters": {
  "alt": {
   "type": "string",
   "description": "Data format for the response.",
   "default": "atom",
   "enum": [
    "atom",
    "json"
   ],
   "enumDescriptions": [
    "Responses with Content-Type of application/atom+xml",
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
    "https://www.googleapis.com/auth/apps.groups.settings": {
     "description": "View and manage the settings of a Google Apps Group"
    }
   }
  }
 },
 "schemas": {
  "Groups": {
   "id": "Groups",
   "type": "object",
   "description": "JSON template for Group resource",
   "properties": {
    "allowExternalMembers": {
     "type": "string",
     "description": "Are external members allowed to join the group."
    },
    "allowGoogleCommunication": {
     "type": "string",
     "description": "Is google allowed to contact admins."
    },
    "allowWebPosting": {
     "type": "string",
     "description": "If posting from web is allowed."
    },
    "archiveOnly": {
     "type": "string",
     "description": "If the group is archive only"
    },
    "customReplyTo": {
     "type": "string",
     "description": "Default email to which reply to any message should go."
    },
    "defaultMessageDenyNotificationText": {
     "type": "string",
     "description": "Default message deny notification message"
    },
    "description": {
     "type": "string",
     "description": "Description of the group"
    },
    "email": {
     "type": "string",
     "description": "Email id of the group"
    },
    "includeInGlobalAddressList": {
     "type": "string",
     "description": "If this groups should be included in global address list or not."
    },
    "isArchived": {
     "type": "string",
     "description": "If the contents of the group are archived."
    },
    "kind": {
     "type": "string",
     "description": "The type of the resource.",
     "default": "groupsSettings#groups"
    },
    "maxMessageBytes": {
     "type": "integer",
     "description": "Maximum message size allowed.",
     "format": "int32"
    },
    "membersCanPostAsTheGroup": {
     "type": "string",
     "description": "Can members post using the group email address."
    },
    "messageDisplayFont": {
     "type": "string",
     "description": "Default message display font. Possible values are: DEFAULT_FONT FIXED_WIDTH_FONT"
    },
    "messageModerationLevel": {
     "type": "string",
     "description": "Moderation level for messages. Possible values are: MODERATE_ALL_MESSAGES MODERATE_NON_MEMBERS MODERATE_NEW_MEMBERS MODERATE_NONE"
    },
    "name": {
     "type": "string",
     "description": "Name of the Group"
    },
    "primaryLanguage": {
     "type": "string",
     "description": "Primary language for the group."
    },
    "replyTo": {
     "type": "string",
     "description": "Whome should the default reply to a message go to. Possible values are: REPLY_TO_CUSTOM REPLY_TO_SENDER REPLY_TO_LIST REPLY_TO_OWNER REPLY_TO_IGNORE REPLY_TO_MANAGERS"
    },
    "sendMessageDenyNotification": {
     "type": "string",
     "description": "Should the member be notified if his message is denied by owner."
    },
    "showInGroupDirectory": {
     "type": "string",
     "description": "Is the group listed in groups directory"
    },
    "spamModerationLevel": {
     "type": "string",
     "description": "Moderation level for messages detected as spam. Possible values are: ALLOW MODERATE SILENTLY_MODERATE REJECT"
    },
    "whoCanInvite": {
     "type": "string",
     "description": "Permissions to invite members. Possbile values are: ALL_MEMBERS_CAN_INVITE ALL_MANAGERS_CAN_INVITE"
    },
    "whoCanJoin": {
     "type": "string",
     "description": "Permissions to join the group. Possible values are: ANYONE_CAN_JOIN ALL_IN_DOMAIN_CAN_JOIN INVITED_CAN_JOIN CAN_REQUEST_TO_JOIN"
    },
    "whoCanPostMessage": {
     "type": "string",
     "description": "Permissions to post messages to the group. Possible values are: NONE_CAN_POST ALL_MANAGERS_CAN_POST ALL_MEMBERS_CAN_POST ALL_IN_DOMAIN_CAN_POST ANYONE_CAN_POST"
    },
    "whoCanViewGroup": {
     "type": "string",
     "description": "Permissions to view group. Possbile values are: ANYONE_CAN_VIEW ALL_IN_DOMAIN_CAN_VIEW ALL_MEMBERS_CAN_VIEW ALL_MANAGERS_CAN_VIEW"
    },
    "whoCanViewMembership": {
     "type": "string",
     "description": "Permissions to view membership. Possbile values are: ALL_IN_DOMAIN_CAN_VIEW ALL_MEMBERS_CAN_VIEW ALL_MANAGERS_CAN_VIEW"
    }
   }
  }
 },
 "resources": {
  "groups": {
   "methods": {
    "get": {
     "id": "groupsSettings.groups.get",
     "path": "{groupUniqueId}",
     "httpMethod": "GET",
     "description": "Gets one resource by id.",
     "parameters": {
      "groupUniqueId": {
       "type": "string",
       "description": "The resource ID",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "groupUniqueId"
     ],
     "response": {
      "$ref": "Groups"
     },
     "scopes": [
      "https://www.googleapis.com/auth/apps.groups.settings"
     ]
    },
    "patch": {
     "id": "groupsSettings.groups.patch",
     "path": "{groupUniqueId}",
     "httpMethod": "PATCH",
     "description": "Updates an existing resource. This method supports patch semantics.",
     "parameters": {
      "groupUniqueId": {
       "type": "string",
       "description": "The resource ID",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "groupUniqueId"
     ],
     "request": {
      "$ref": "Groups"
     },
     "response": {
      "$ref": "Groups"
     },
     "scopes": [
      "https://www.googleapis.com/auth/apps.groups.settings"
     ]
    },
    "update": {
     "id": "groupsSettings.groups.update",
     "path": "{groupUniqueId}",
     "httpMethod": "PUT",
     "description": "Updates an existing resource.",
     "parameters": {
      "groupUniqueId": {
       "type": "string",
       "description": "The resource ID",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "groupUniqueId"
     ],
     "request": {
      "$ref": "Groups"
     },
     "response": {
      "$ref": "Groups"
     },
     "scopes": [
      "https://www.googleapis.com/auth/apps.groups.settings"
     ]
    }
   }
  }
 }
}
