{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "orkut:v2",
 "name": "orkut",
 "version": "v2",
 "revision": "20120223",
 "title": "Orkut API",
 "description": "Lets you manage activities, comments and badges in Orkut. More stuff coming in time.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/orkut-16.png",
  "x32": "http://www.google.com/images/icons/product/orkut-32.png"
 },
 "documentationLink": "http://code.google.com/apis/orkut/v2/reference.html",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/orkut/v2/",
 "basePath": "/orkut/v2/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "orkut/v2/",
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
    "https://www.googleapis.com/auth/orkut": {
     "description": "Manage your Orkut activity"
    },
    "https://www.googleapis.com/auth/orkut.readonly": {
     "description": "View your Orkut data"
    }
   }
  }
 },
 "schemas": {
  "Acl": {
   "id": "Acl",
   "type": "object",
   "properties": {
    "description": {
     "type": "string",
     "description": "Human readable description of the access granted."
    },
    "items": {
     "type": "array",
     "description": "The list of ACL entries.",
     "items": {
      "type": "object",
      "properties": {
       "id": {
        "type": "string",
        "description": "The ID of the entity. For entities of type \"person\" or \"circle\", this is the ID of the resource. For other types, this will be unset."
       },
       "type": {
        "type": "string",
        "description": "The type of entity to whom access is granted."
       }
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as an access control list. Value: \"orkut#acl\"",
     "default": "orkut#acl"
    },
    "totalParticipants": {
     "type": "integer",
     "description": "The total count of participants of the parent resource.",
     "format": "int32"
    }
   }
  },
  "Activity": {
   "id": "Activity",
   "type": "object",
   "properties": {
    "access": {
     "$ref": "Acl",
     "description": "Identifies who has access to see this activity."
    },
    "actor": {
     "$ref": "OrkutAuthorResource",
     "description": "The person who performed the activity."
    },
    "id": {
     "type": "string",
     "description": "The ID for the activity."
    },
    "kind": {
     "type": "string",
     "description": "The kind of activity. Always orkut#activity.",
     "default": "orkut#activity"
    },
    "links": {
     "type": "array",
     "description": "Links to resources related to this activity.",
     "items": {
      "$ref": "OrkutLinkResource"
     }
    },
    "object": {
     "type": "object",
     "description": "The activity's object.",
     "properties": {
      "content": {
       "type": "string",
       "description": "The HTML-formatted content, suitable for display. When updating an activity's content, post the changes to this property, using the value of originalContent as a starting point. If the update is successful, the server adds HTML formatting and responds with this formatted content."
      },
      "items": {
       "type": "array",
       "description": "The list of additional items.",
       "items": {
        "$ref": "OrkutActivityobjectsResource"
       }
      },
      "objectType": {
       "type": "string",
       "description": "The type of the object affected by the activity. Clients can use this information to style the rendered activity object differently depending on the content."
      },
      "replies": {
       "type": "object",
       "description": "Comments in reply to this activity.",
       "properties": {
        "items": {
         "type": "array",
         "description": "The list of comments.",
         "items": {
          "$ref": "Comment"
         }
        },
        "totalItems": {
         "type": "string",
         "description": "Total number of comments.",
         "format": "uint64"
        },
        "url": {
         "type": "string",
         "description": "URL for the collection of comments in reply to this activity."
        }
       }
      }
     }
    },
    "published": {
     "type": "string",
     "description": "The time at which the activity was initially published.",
     "format": "date-time"
    },
    "title": {
     "type": "string",
     "description": "Title of the activity."
    },
    "updated": {
     "type": "string",
     "description": "The time at which the activity was last updated.",
     "format": "date-time"
    },
    "verb": {
     "type": "string",
     "description": "This activity's verb, indicating what action was performed. Possible values are:  \n- add - User added new content to profile or album, e.g. video, photo. \n- post - User publish content to the stream, e.g. status, scrap. \n- update - User commented on an activity. \n- make-friend - User added a new friend. \n- birthday - User has a birthday."
    }
   }
  },
  "ActivityList": {
   "id": "ActivityList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of activities retrieved.",
     "items": {
      "$ref": "Activity"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a collection of activities. Value: \"orkut#activityList\"",
     "default": "orkut#activityList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in activities.list request to get the next page, if there are more to retrieve."
    }
   }
  },
  "Badge": {
   "id": "Badge",
   "type": "object",
   "properties": {
    "badgeLargeLogo": {
     "type": "string",
     "description": "The URL for the 64x64 badge logo."
    },
    "badgeSmallLogo": {
     "type": "string",
     "description": "The URL for the 24x24 badge logo."
    },
    "caption": {
     "type": "string",
     "description": "The name of the badge, suitable for display."
    },
    "description": {
     "type": "string",
     "description": "The description for the badge, suitable for display."
    },
    "id": {
     "type": "string",
     "description": "The unique ID for the badge.",
     "format": "int64"
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a badge. Value: \"orkut#badge\"",
     "default": "orkut#badge"
    },
    "sponsorLogo": {
     "type": "string",
     "description": "The URL for the 32x32 badge sponsor logo."
    },
    "sponsorName": {
     "type": "string",
     "description": "The name of the badge sponsor, suitable for display."
    },
    "sponsorUrl": {
     "type": "string",
     "description": "The URL for the badge sponsor."
    }
   }
  },
  "BadgeList": {
   "id": "BadgeList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of badges retrieved.",
     "items": {
      "$ref": "Badge"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a collection of badges. Value: \"orkut#badgeList\"",
     "default": "orkut#badgeList"
    }
   }
  },
  "Comment": {
   "id": "Comment",
   "type": "object",
   "properties": {
    "actor": {
     "$ref": "OrkutAuthorResource",
     "description": "The person who posted the comment."
    },
    "content": {
     "type": "string",
     "description": "The content of the comment in text/html"
    },
    "id": {
     "type": "string",
     "description": "The unique ID for the comment."
    },
    "inReplyTo": {
     "type": "object",
     "description": "Link to the original activity where this comment was posted.",
     "properties": {
      "href": {
       "type": "string",
       "description": "Link to the post on activity stream being commented."
      },
      "ref": {
       "type": "string",
       "description": "Unique identifier of the post on activity stream being commented."
      },
      "rel": {
       "type": "string",
       "description": "Relationship between the comment and the post on activity stream being commented. Always inReplyTo.",
       "default": "inReplyTo"
      },
      "type": {
       "type": "string",
       "description": "Type of the post on activity stream being commented. Always text/html."
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a comment. Value: \"orkut#comment\"",
     "default": "orkut#comment"
    },
    "links": {
     "type": "array",
     "description": "List of resources for the comment.",
     "items": {
      "$ref": "OrkutLinkResource"
     }
    },
    "published": {
     "type": "string",
     "description": "The time the comment was initially published, in RFC 3339 format.",
     "format": "date-time"
    }
   }
  },
  "CommentList": {
   "id": "CommentList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of comments retrieved.",
     "items": {
      "$ref": "Comment"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a collection of comments. Value: \"orkut#commentList\"",
     "default": "orkut#commentList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in comments.list request to get the next page, if there are more to retrieve."
    },
    "previousPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in comments.list request to get the previous page, if there are more to retrieve."
    }
   }
  },
  "Community": {
   "id": "Community",
   "type": "object",
   "properties": {
    "category": {
     "type": "string",
     "description": "The category of the community."
    },
    "co_owners": {
     "type": "array",
     "description": "The co-owners of the community.",
     "items": {
      "$ref": "OrkutAuthorResource"
     }
    },
    "creation_date": {
     "type": "string",
     "description": "The time the community was created, in RFC 3339 format.",
     "format": "date-time"
    },
    "description": {
     "type": "string",
     "description": "The description of the community."
    },
    "id": {
     "type": "integer",
     "description": "The id of the community.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a community. Value: \"orkut#community\"",
     "default": "orkut#community"
    },
    "language": {
     "type": "string",
     "description": "The official language of the community."
    },
    "links": {
     "type": "array",
     "description": "List of resources for the community.",
     "items": {
      "$ref": "OrkutLinkResource"
     }
    },
    "location": {
     "type": "string",
     "description": "The location of the community."
    },
    "member_count": {
     "type": "integer",
     "description": "The number of users who are part of the community. This number may be approximate, so do not rely on it for iteration.",
     "format": "int32"
    },
    "moderators": {
     "type": "array",
     "description": "The list of moderators of the community.",
     "items": {
      "$ref": "OrkutAuthorResource"
     }
    },
    "name": {
     "type": "string",
     "description": "The name of the community."
    },
    "owner": {
     "$ref": "OrkutAuthorResource",
     "description": "The person who owns the community."
    },
    "photo_url": {
     "type": "string",
     "description": "The photo of the community."
    }
   }
  },
  "CommunityList": {
   "id": "CommunityList",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of communities retrieved.",
     "items": {
      "$ref": "Community"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a collection of communities. Value: \"orkut#communityList\"",
     "default": "orkut#communityList"
    }
   }
  },
  "CommunityMembers": {
   "id": "CommunityMembers",
   "type": "object",
   "properties": {
    "communityMembershipStatus": {
     "$ref": "CommunityMembershipStatus",
     "description": "Status and permissions of the user related to the community."
    },
    "kind": {
     "type": "string",
     "description": "Kind of this item. Always orkut#communityMembers.",
     "default": "orkut#communityMembers"
    },
    "person": {
     "$ref": "OrkutActivitypersonResource",
     "description": "Description of the community member."
    }
   }
  },
  "CommunityMembersList": {
   "id": "CommunityMembersList",
   "type": "object",
   "properties": {
    "firstPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_members.list request to get the first page."
    },
    "items": {
     "type": "array",
     "description": "List of community members retrieved.",
     "items": {
      "$ref": "CommunityMembers"
     }
    },
    "kind": {
     "type": "string",
     "description": "Kind of this item. Always orkut#communityMembersList.",
     "default": "orkut#communityMembersList"
    },
    "lastPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_members.list request to get the last page."
    },
    "nextPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_members.list request to get the next page, if there are more to retrieve."
    },
    "prevPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_members.list request to get the previous page, if there are more to retrieve."
    }
   }
  },
  "CommunityMembershipStatus": {
   "id": "CommunityMembershipStatus",
   "type": "object",
   "properties": {
    "canCreatePoll": {
     "type": "boolean",
     "description": "Whether the user can create a poll in this community."
    },
    "canCreateTopic": {
     "type": "boolean",
     "description": "Whether the user can create a topic in this community."
    },
    "canShout": {
     "type": "boolean",
     "description": "Whether the user can perform a shout operation in this community."
    },
    "isCoOwner": {
     "type": "boolean",
     "description": "Whether the session user is a community co-owner."
    },
    "isFollowing": {
     "type": "boolean",
     "description": "Whether the user is following this community."
    },
    "isModerator": {
     "type": "boolean",
     "description": "Whether the session user is a community moderator."
    },
    "isOwner": {
     "type": "boolean",
     "description": "Whether the session user is the community owner."
    },
    "isRestoreAvailable": {
     "type": "boolean",
     "description": "Whether the restore operation is available for the community."
    },
    "isTakebackAvailable": {
     "type": "boolean",
     "description": "Whether the take-back operation is available for the community."
    },
    "kind": {
     "type": "string",
     "description": "Kind of this item. Always orkut#communityMembershipStatus.",
     "default": "orkut#communityMembershipStatus"
    },
    "status": {
     "type": "string",
     "description": "The status of the current link between the community and the user."
    }
   }
  },
  "CommunityMessage": {
   "id": "CommunityMessage",
   "type": "object",
   "properties": {
    "addedDate": {
     "type": "string",
     "description": "The timestamp of the date when the message was added, in RFC 3339 format.",
     "format": "date-time"
    },
    "author": {
     "$ref": "OrkutAuthorResource",
     "description": "The creator of the message. If ommited, the message is annonimous."
    },
    "body": {
     "type": "string",
     "description": "The body of the message."
    },
    "id": {
     "type": "string",
     "description": "The ID of the message.",
     "format": "int64"
    },
    "isSpam": {
     "type": "boolean",
     "description": "Whether this post was marked as spam by the viewer, when he/she is not the community owner or one of its moderators."
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a community message. Value: \"orkut#communityMessage\"",
     "default": "orkut#communityMessage"
    },
    "links": {
     "type": "array",
     "description": "List of resources for the community message.",
     "items": {
      "$ref": "OrkutLinkResource"
     }
    },
    "subject": {
     "type": "string",
     "description": "The subject of the message."
    }
   }
  },
  "CommunityMessageList": {
   "id": "CommunityMessageList",
   "type": "object",
   "properties": {
    "firstPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_messages.list request to get the first page."
    },
    "items": {
     "type": "array",
     "description": "List of messages retrieved.",
     "items": {
      "$ref": "CommunityMessage"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a collection of community messages. Value: \"orkut#communityMessageList\"",
     "default": "orkut#communityMessageList"
    },
    "lastPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_messages.list request to get the last page."
    },
    "nextPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_messages.list request to get the next page, if there are more to retrieve."
    },
    "prevPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_messages.list request to get the previous page, if there are more to retrieve."
    }
   }
  },
  "CommunityPoll": {
   "id": "CommunityPoll",
   "type": "object",
   "properties": {
    "author": {
     "$ref": "OrkutAuthorResource",
     "description": "The person who created the poll."
    },
    "communityId": {
     "type": "integer",
     "description": "The ID of the community.",
     "format": "int32"
    },
    "creationTime": {
     "type": "string",
     "description": "The date of creation of this poll",
     "format": "date-time"
    },
    "description": {
     "type": "string",
     "description": "The poll description."
    },
    "endingTime": {
     "type": "string",
     "description": "The ending date of this poll or empty if the poll doesn't have one.",
     "format": "date-time"
    },
    "hasVoted": {
     "type": "boolean",
     "description": "Whether the user has voted on this poll."
    },
    "id": {
     "type": "string",
     "description": "The poll ID."
    },
    "image": {
     "type": "object",
     "description": "The image representing the poll. Field is omitted if no image exists.",
     "properties": {
      "url": {
       "type": "string",
       "description": "A URL that points to an image of the poll."
      }
     }
    },
    "isClosed": {
     "type": "boolean",
     "description": "Whether the poll is not expired if there is an expiration date. A poll is open (that is, not closed for voting) if it either is not expired or doesn't have an expiration date at all. Note that just because a poll is open, it doesn't mean that the requester can vote on it."
    },
    "isMultipleAnswers": {
     "type": "boolean",
     "description": "Whether this poll allows voting for more than one option."
    },
    "isOpenForVoting": {
     "type": "boolean",
     "description": "Whether this poll is still opened for voting. A poll is open for voting if it is not closed, the user has not yet voted on it and the user has the permission to do so, which happens if he/she is either a community member or the poll is open for everybody."
    },
    "isRestricted": {
     "type": "boolean",
     "description": "Whether this poll is restricted for members only. If a poll is open but the user can't vote on it, it's been restricted to members only. This information is important to tell this case apart from the one where the user can't vote simply because the poll is already closed."
    },
    "isSpam": {
     "type": "boolean",
     "description": "Whether the user has marked this poll as spam. This only affects the poll for this user, not globally."
    },
    "isUsersVotePublic": {
     "type": "boolean",
     "description": "If user has already voted, whether his vote is publicly visible."
    },
    "isVotingAllowedForNonMembers": {
     "type": "boolean",
     "description": "Whether non-members of the community can vote on the poll."
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a community poll. Value: \"orkut#communityPoll\"",
     "default": "orkut#communityPoll"
    },
    "lastUpdate": {
     "type": "string",
     "description": "The date of the last update of this poll.",
     "format": "date-time"
    },
    "links": {
     "type": "array",
     "description": "List of resources for the community poll.",
     "items": {
      "$ref": "OrkutLinkResource"
     }
    },
    "options": {
     "type": "array",
     "description": "List of options of this poll.",
     "items": {
      "$ref": "OrkutCommunitypolloptionResource"
     }
    },
    "question": {
     "type": "string",
     "description": "The poll question."
    },
    "totalNumberOfVotes": {
     "type": "integer",
     "description": "The total number of votes this poll has received.",
     "format": "int32"
    },
    "votedOptions": {
     "type": "array",
     "description": "List of options the user has voted on, if there are any.",
     "items": {
      "type": "integer",
      "format": "int32"
     }
    }
   }
  },
  "CommunityPollComment": {
   "id": "CommunityPollComment",
   "type": "object",
   "properties": {
    "addedDate": {
     "type": "string",
     "description": "The date when the message was added, in RFC 3339 format.",
     "format": "date-time"
    },
    "author": {
     "$ref": "OrkutAuthorResource",
     "description": "The creator of the comment."
    },
    "body": {
     "type": "string",
     "description": "The body of the message."
    },
    "id": {
     "type": "integer",
     "description": "The ID of the comment.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a community poll comment. Value: \"orkut#communityPollComment\"",
     "default": "orkut#communityPollComment"
    }
   }
  },
  "CommunityPollCommentList": {
   "id": "CommunityPollCommentList",
   "type": "object",
   "properties": {
    "firstPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_poll_comments.list request to get the first page."
    },
    "items": {
     "type": "array",
     "description": "List of community poll comments retrieved.",
     "items": {
      "$ref": "CommunityPollComment"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a collection of community poll comments. Value: \"orkut#CommunityPollCommentList\"",
     "default": "orkut#CommunityPollCommentList"
    },
    "lastPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_poll_comments.list request to get the last page."
    },
    "nextPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_poll_comments.list request to get the next page, if there are more to retrieve."
    },
    "prevPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_poll_comments.list request to get the previous page, if there are more to retrieve."
    }
   }
  },
  "CommunityPollList": {
   "id": "CommunityPollList",
   "type": "object",
   "properties": {
    "firstPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_polls.list request to get the first page."
    },
    "items": {
     "type": "array",
     "description": "List of community polls retrieved.",
     "items": {
      "$ref": "CommunityPoll"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a collection of community polls. Value: \"orkut#communityPollList\"",
     "default": "orkut#communityPollList"
    },
    "lastPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_polls.list request to get the last page."
    },
    "nextPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_polls.list request to get the next page, if there are more to retrieve."
    },
    "prevPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_polls.list request to get the previous page, if there are more to retrieve."
    }
   }
  },
  "CommunityPollVote": {
   "id": "CommunityPollVote",
   "type": "object",
   "properties": {
    "isVotevisible": {
     "type": "boolean",
     "description": "Whether this vote is visible to other users or not."
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a community poll vote. Value: \"orkut#communityPollVote\"",
     "default": "orkut#communityPollVote"
    },
    "optionIds": {
     "type": "array",
     "description": "The ids of the voted options.",
     "items": {
      "type": "integer",
      "format": "int32"
     }
    }
   }
  },
  "CommunityTopic": {
   "id": "CommunityTopic",
   "type": "object",
   "properties": {
    "author": {
     "$ref": "OrkutAuthorResource",
     "description": "The creator of the topic."
    },
    "body": {
     "type": "string",
     "description": "The body of the topic."
    },
    "id": {
     "type": "string",
     "description": "The ID of the topic.",
     "format": "int64"
    },
    "isClosed": {
     "type": "boolean",
     "description": "Whether the topic is closed for new messages."
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a community topic. Value: \"orkut#communityTopic\"",
     "default": "orkut#communityTopic"
    },
    "lastUpdate": {
     "type": "string",
     "description": "The timestamp of the last update, in RFC 3339 format.",
     "format": "date-time"
    },
    "latestMessageSnippet": {
     "type": "string",
     "description": "Snippet of the last message posted on this topic."
    },
    "links": {
     "type": "array",
     "description": "List of resources for the community.",
     "items": {
      "$ref": "OrkutLinkResource"
     }
    },
    "messages": {
     "type": "array",
     "description": "Most recent messages.",
     "items": {
      "$ref": "CommunityMessage"
     }
    },
    "numberOfReplies": {
     "type": "integer",
     "description": "The total number of replies this topic has received.",
     "format": "int32"
    },
    "title": {
     "type": "string",
     "description": "The title of the topic."
    }
   }
  },
  "CommunityTopicList": {
   "id": "CommunityTopicList",
   "type": "object",
   "properties": {
    "firstPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_topic.list request to get the first page."
    },
    "items": {
     "type": "array",
     "description": "List of topics retrieved.",
     "items": {
      "$ref": "CommunityTopic"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a collection of community topics. Value: \"orkut#communityTopicList\"",
     "default": "orkut#communityTopicList"
    },
    "lastPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_topic.list request to get the last page."
    },
    "nextPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_topic.list request to get the next page, if there are more to retrieve."
    },
    "prevPageToken": {
     "type": "string",
     "description": "The value of pageToken query parameter in community_topic.list request to get the previous page, if there are more to retrieve."
    }
   }
  },
  "Counters": {
   "id": "Counters",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "List of counters retrieved.",
     "items": {
      "$ref": "OrkutCounterResource"
     }
    },
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a collection of counters. Value: \"orkut#counters\"",
     "default": "orkut#counters"
    }
   }
  },
  "OrkutActivityobjectsResource": {
   "id": "OrkutActivityobjectsResource",
   "type": "object",
   "properties": {
    "community": {
     "$ref": "Community",
     "description": "The community which is related with this activity, e.g. a joined community."
    },
    "content": {
     "type": "string",
     "description": "The HTML-formatted content, suitable for display. When updating an activity's content, post the changes to this property, using the value of originalContent as a starting point. If the update is successful, the server adds HTML formatting and responds with this formatted content."
    },
    "displayName": {
     "type": "string",
     "description": "The title of the object."
    },
    "id": {
     "type": "string",
     "description": "The ID for the object."
    },
    "links": {
     "type": "array",
     "description": "Links to other resources related to this object.",
     "items": {
      "$ref": "OrkutLinkResource"
     }
    },
    "objectType": {
     "type": "string",
     "description": "The object type."
    },
    "person": {
     "$ref": "OrkutActivitypersonResource",
     "description": "The person who is related with this activity, e.g. an Added User."
    }
   }
  },
  "OrkutActivitypersonResource": {
   "id": "OrkutActivitypersonResource",
   "type": "object",
   "properties": {
    "birthday": {
     "type": "string",
     "description": "The person's date of birth, represented as YYYY-MM-DD."
    },
    "gender": {
     "type": "string",
     "description": "The person's gender. Values include \"male\", \"female\", and \"other\"."
    },
    "id": {
     "type": "string",
     "description": "The person's opensocial ID."
    },
    "image": {
     "type": "object",
     "description": "The person's profile photo. This is adapted from Google+ and was originaly introduced as extra OpenSocial convenience fields.",
     "properties": {
      "url": {
       "type": "string",
       "description": "The URL of the person's profile photo."
      }
     }
    },
    "name": {
     "type": "object",
     "description": "An object that encapsulates the individual components of a person's name.",
     "properties": {
      "familyName": {
       "type": "string",
       "description": "The family name (last name) of this person."
      },
      "givenName": {
       "type": "string",
       "description": "The given name (first name) of this person."
      }
     }
    },
    "url": {
     "type": "string",
     "description": "The person's profile url. This is adapted from Google+ and was originaly introduced as extra OpenSocial convenience fields."
    }
   }
  },
  "OrkutAuthorResource": {
   "id": "OrkutAuthorResource",
   "type": "object",
   "properties": {
    "displayName": {
     "type": "string",
     "description": "The name of the author, suitable for display."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier of the person who posted the comment. This is the person's OpenSocial ID."
    },
    "image": {
     "type": "object",
     "description": "Image data about the author.",
     "properties": {
      "url": {
       "type": "string",
       "description": "A URL that points to a thumbnail photo of the author."
      }
     }
    },
    "url": {
     "type": "string",
     "description": "The URL of the author who posted the comment [not yet implemented]"
    }
   }
  },
  "OrkutCommunitypolloptionResource": {
   "id": "OrkutCommunitypolloptionResource",
   "type": "object",
   "properties": {
    "description": {
     "type": "string",
     "description": "The option description."
    },
    "image": {
     "type": "object",
     "description": "Image data about the poll option. Field is omitted if no image exists.",
     "properties": {
      "url": {
       "type": "string",
       "description": "A URL that points to an image of the poll question."
      }
     }
    },
    "numberOfVotes": {
     "type": "integer",
     "description": "The total number of votes that this option received.",
     "format": "int32"
    },
    "optionId": {
     "type": "integer",
     "description": "The poll option ID",
     "format": "int32"
    }
   }
  },
  "OrkutCounterResource": {
   "id": "OrkutCounterResource",
   "type": "object",
   "properties": {
    "link": {
     "$ref": "OrkutLinkResource",
     "description": "Link to the collection being counted."
    },
    "name": {
     "type": "string",
     "description": "The name of the counted collection. Currently supported collections are:  \n- scraps - The scraps of the user. \n- photos - The photos of the user. \n- videos - The videos of the user. \n- pendingTestimonials - The pending testimonials of the user."
    },
    "total": {
     "type": "integer",
     "description": "The number of resources on the counted collection.",
     "format": "int32"
    }
   }
  },
  "OrkutLinkResource": {
   "id": "OrkutLinkResource",
   "type": "object",
   "description": "Links to resources related to the parent object.",
   "properties": {
    "href": {
     "type": "string",
     "description": "URL of the link."
    },
    "rel": {
     "type": "string",
     "description": "Relation between the resource and the parent object."
    },
    "title": {
     "type": "string",
     "description": "Title of the link."
    },
    "type": {
     "type": "string",
     "description": "Media type of the link."
    }
   }
  },
  "Visibility": {
   "id": "Visibility",
   "type": "object",
   "properties": {
    "kind": {
     "type": "string",
     "description": "Identifies this resource as a visibility item. Value: \"orkut#visibility\"",
     "default": "orkut#visibility"
    },
    "links": {
     "type": "array",
     "description": "List of resources for the visibility item.",
     "items": {
      "$ref": "OrkutLinkResource"
     }
    },
    "visibility": {
     "type": "string",
     "description": "The visibility of the resource. Possible values are:  \n- default: not hidden by the user \n- hidden: hidden"
    }
   }
  }
 },
 "resources": {
  "acl": {
   "methods": {
    "delete": {
     "id": "orkut.acl.delete",
     "path": "activities/{activityId}/acl/{userId}",
     "httpMethod": "DELETE",
     "description": "Excludes an element from the ACL of the activity.",
     "parameters": {
      "activityId": {
       "type": "string",
       "description": "ID of the activity.",
       "required": true,
       "location": "path"
      },
      "userId": {
       "type": "string",
       "description": "ID of the user to be removed from the activity.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "activityId",
      "userId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    }
   }
  },
  "activities": {
   "methods": {
    "delete": {
     "id": "orkut.activities.delete",
     "path": "activities/{activityId}",
     "httpMethod": "DELETE",
     "description": "Deletes an existing activity, if the access controls allow it.",
     "parameters": {
      "activityId": {
       "type": "string",
       "description": "ID of the activity to remove.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "activityId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "list": {
     "id": "orkut.activities.list",
     "path": "people/{userId}/activities/{collection}",
     "httpMethod": "GET",
     "description": "Retrieves a list of activities.",
     "parameters": {
      "collection": {
       "type": "string",
       "description": "The collection of activities to list.",
       "required": true,
       "enum": [
        "all",
        "scraps",
        "stream"
       ],
       "enumDescriptions": [
        "All activities created by the specified user that the authenticated user is authorized to view.",
        "The specified user's scrapbook.",
        "The specified user's stream feed, intended for consumption. This includes activities posted by people that the user is following, and activities in which the user has been mentioned."
       ],
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of activities to include in the response.",
       "format": "uint32",
       "minimum": "1",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token that allows pagination.",
       "location": "query"
      },
      "userId": {
       "type": "string",
       "description": "The ID of the user whose activities will be listed. Can be me to refer to the viewer (i.e. the authenticated user).",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "userId",
      "collection"
     ],
     "response": {
      "$ref": "ActivityList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "activityVisibility": {
   "methods": {
    "get": {
     "id": "orkut.activityVisibility.get",
     "path": "activities/{activityId}/visibility",
     "httpMethod": "GET",
     "description": "Gets the visibility of an existing activity.",
     "parameters": {
      "activityId": {
       "type": "string",
       "description": "ID of the activity to get the visibility.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "activityId"
     ],
     "response": {
      "$ref": "Visibility"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    },
    "patch": {
     "id": "orkut.activityVisibility.patch",
     "path": "activities/{activityId}/visibility",
     "httpMethod": "PATCH",
     "description": "Updates the visibility of an existing activity. This method supports patch semantics.",
     "parameters": {
      "activityId": {
       "type": "string",
       "description": "ID of the activity.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "activityId"
     ],
     "request": {
      "$ref": "Visibility"
     },
     "response": {
      "$ref": "Visibility"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "update": {
     "id": "orkut.activityVisibility.update",
     "path": "activities/{activityId}/visibility",
     "httpMethod": "PUT",
     "description": "Updates the visibility of an existing activity.",
     "parameters": {
      "activityId": {
       "type": "string",
       "description": "ID of the activity.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "activityId"
     ],
     "request": {
      "$ref": "Visibility"
     },
     "response": {
      "$ref": "Visibility"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    }
   }
  },
  "badges": {
   "methods": {
    "get": {
     "id": "orkut.badges.get",
     "path": "people/{userId}/badges/{badgeId}",
     "httpMethod": "GET",
     "description": "Retrieves a badge from a user.",
     "parameters": {
      "badgeId": {
       "type": "string",
       "description": "The ID of the badge that will be retrieved.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "userId": {
       "type": "string",
       "description": "The ID of the user whose badges will be listed. Can be me to refer to caller.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "userId",
      "badgeId"
     ],
     "response": {
      "$ref": "Badge"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    },
    "list": {
     "id": "orkut.badges.list",
     "path": "people/{userId}/badges",
     "httpMethod": "GET",
     "description": "Retrieves the list of visible badges of a user.",
     "parameters": {
      "userId": {
       "type": "string",
       "description": "The id of the user whose badges will be listed. Can be me to refer to caller.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "userId"
     ],
     "response": {
      "$ref": "BadgeList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "comments": {
   "methods": {
    "delete": {
     "id": "orkut.comments.delete",
     "path": "comments/{commentId}",
     "httpMethod": "DELETE",
     "description": "Deletes an existing comment.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "ID of the comment to remove.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "commentId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "get": {
     "id": "orkut.comments.get",
     "path": "comments/{commentId}",
     "httpMethod": "GET",
     "description": "Retrieves an existing comment.",
     "parameters": {
      "commentId": {
       "type": "string",
       "description": "ID of the comment to get.",
       "required": true,
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "commentId"
     ],
     "response": {
      "$ref": "Comment"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    },
    "insert": {
     "id": "orkut.comments.insert",
     "path": "activities/{activityId}/comments",
     "httpMethod": "POST",
     "description": "Inserts a new comment to an activity.",
     "parameters": {
      "activityId": {
       "type": "string",
       "description": "The ID of the activity to contain the new comment.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "activityId"
     ],
     "request": {
      "$ref": "Comment"
     },
     "response": {
      "$ref": "Comment"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "list": {
     "id": "orkut.comments.list",
     "path": "activities/{activityId}/comments",
     "httpMethod": "GET",
     "description": "Retrieves a list of comments, possibly filtered.",
     "parameters": {
      "activityId": {
       "type": "string",
       "description": "The ID of the activity containing the comments.",
       "required": true,
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of activities to include in the response.",
       "format": "uint32",
       "minimum": "1",
       "location": "query"
      },
      "orderBy": {
       "type": "string",
       "description": "Sort search results.",
       "default": "DESCENDING_SORT",
       "enum": [
        "ascending",
        "descending"
       ],
       "enumDescriptions": [
        "Use ascending sort order.",
        "Use descending sort order."
       ],
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token that allows pagination.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "activityId"
     ],
     "response": {
      "$ref": "CommentList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "communities": {
   "methods": {
    "get": {
     "id": "orkut.communities.get",
     "path": "communities/{communityId}",
     "httpMethod": "GET",
     "description": "Retrieves the basic information (aka. profile) of a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community to get.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "communityId"
     ],
     "response": {
      "$ref": "Community"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    },
    "list": {
     "id": "orkut.communities.list",
     "path": "people/{userId}/communities",
     "httpMethod": "GET",
     "description": "Retrieves the list of communities the current user is a member of.",
     "parameters": {
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of communities to include in the response.",
       "format": "uint32",
       "minimum": "1",
       "location": "query"
      },
      "orderBy": {
       "type": "string",
       "description": "How to order the communities by.",
       "enum": [
        "id",
        "ranked"
       ],
       "enumDescriptions": [
        "Returns the communities sorted by a fixed, natural order.",
        "Returns the communities ranked accordingly to how they are displayed on the orkut web application."
       ],
       "location": "query"
      },
      "userId": {
       "type": "string",
       "description": "The ID of the user whose communities will be listed. Can be me to refer to caller.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "userId"
     ],
     "response": {
      "$ref": "CommunityList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "communityFollow": {
   "methods": {
    "delete": {
     "id": "orkut.communityFollow.delete",
     "path": "communities/{communityId}/followers/{userId}",
     "httpMethod": "DELETE",
     "description": "Removes a user from the followers of a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "ID of the community.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "userId": {
       "type": "string",
       "description": "ID of the user.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "userId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "insert": {
     "id": "orkut.communityFollow.insert",
     "path": "communities/{communityId}/followers/{userId}",
     "httpMethod": "POST",
     "description": "Adds a user as a follower of a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "ID of the community.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "userId": {
       "type": "string",
       "description": "ID of the user.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "userId"
     ],
     "response": {
      "$ref": "CommunityMembers"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    }
   }
  },
  "communityMembers": {
   "methods": {
    "delete": {
     "id": "orkut.communityMembers.delete",
     "path": "communities/{communityId}/members/{userId}",
     "httpMethod": "DELETE",
     "description": "Makes the user leave a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "ID of the community.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "userId": {
       "type": "string",
       "description": "ID of the user.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "userId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "get": {
     "id": "orkut.communityMembers.get",
     "path": "communities/{communityId}/members/{userId}",
     "httpMethod": "GET",
     "description": "Retrieves the relationship between a user and a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "ID of the community.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "userId": {
       "type": "string",
       "description": "ID of the user.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "userId"
     ],
     "response": {
      "$ref": "CommunityMembers"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    },
    "insert": {
     "id": "orkut.communityMembers.insert",
     "path": "communities/{communityId}/members/{userId}",
     "httpMethod": "POST",
     "description": "Makes the user join a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "ID of the community.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "userId": {
       "type": "string",
       "description": "ID of the user.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "userId"
     ],
     "response": {
      "$ref": "CommunityMembers"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "list": {
     "id": "orkut.communityMembers.list",
     "path": "communities/{communityId}/members",
     "httpMethod": "GET",
     "description": "Lists members of a community. Use the pagination tokens to retrieve the full list; do not rely on the member count available in the community profile information to know when to stop iterating, as that count may be approximate.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community whose members will be listed.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "friendsOnly": {
       "type": "boolean",
       "description": "Whether to list only community members who are friends of the user.",
       "location": "query"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of members to include in the response.",
       "format": "uint32",
       "minimum": "1",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token that allows pagination.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "communityId"
     ],
     "response": {
      "$ref": "CommunityMembersList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "communityMessages": {
   "methods": {
    "delete": {
     "id": "orkut.communityMessages.delete",
     "path": "communities/{communityId}/topics/{topicId}/messages/{messageId}",
     "httpMethod": "DELETE",
     "description": "Moves a message of the community to the trash folder.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community whose message will be moved to the trash folder.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "messageId": {
       "type": "string",
       "description": "The ID of the message to be moved to the trash folder.",
       "required": true,
       "format": "int64",
       "location": "path"
      },
      "topicId": {
       "type": "string",
       "description": "The ID of the topic whose message will be moved to the trash folder.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "topicId",
      "messageId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "insert": {
     "id": "orkut.communityMessages.insert",
     "path": "communities/{communityId}/topics/{topicId}/messages",
     "httpMethod": "POST",
     "description": "Adds a message to a given community topic.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community the message should be added to.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "topicId": {
       "type": "string",
       "description": "The ID of the topic the message should be added to.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "topicId"
     ],
     "request": {
      "$ref": "CommunityMessage"
     },
     "response": {
      "$ref": "CommunityMessage"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "list": {
     "id": "orkut.communityMessages.list",
     "path": "communities/{communityId}/topics/{topicId}/messages",
     "httpMethod": "GET",
     "description": "Retrieves the messages of a topic of a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community which messages will be listed.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of messages to include in the response.",
       "format": "uint32",
       "minimum": "1",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token that allows pagination.",
       "location": "query"
      },
      "topicId": {
       "type": "string",
       "description": "The ID of the topic which messages will be listed.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "topicId"
     ],
     "response": {
      "$ref": "CommunityMessageList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "communityPollComments": {
   "methods": {
    "insert": {
     "id": "orkut.communityPollComments.insert",
     "path": "communities/{communityId}/polls/{pollId}/comments",
     "httpMethod": "POST",
     "description": "Adds a comment on a community poll.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community whose poll is being commented.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "pollId": {
       "type": "string",
       "description": "The ID of the poll being commented.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "pollId"
     ],
     "request": {
      "$ref": "CommunityPollComment"
     },
     "response": {
      "$ref": "CommunityPollComment"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "list": {
     "id": "orkut.communityPollComments.list",
     "path": "communities/{communityId}/polls/{pollId}/comments",
     "httpMethod": "GET",
     "description": "Retrieves the comments of a community poll.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community whose poll is having its comments listed.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of comments to include in the response.",
       "format": "uint32",
       "minimum": "1",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token that allows pagination.",
       "location": "query"
      },
      "pollId": {
       "type": "string",
       "description": "The ID of the community whose polls will be listed.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "pollId"
     ],
     "response": {
      "$ref": "CommunityPollCommentList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "communityPollVotes": {
   "methods": {
    "insert": {
     "id": "orkut.communityPollVotes.insert",
     "path": "communities/{communityId}/polls/{pollId}/votes",
     "httpMethod": "POST",
     "description": "Votes on a community poll.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community whose poll is being voted.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "pollId": {
       "type": "string",
       "description": "The ID of the poll being voted.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "pollId"
     ],
     "request": {
      "$ref": "CommunityPollVote"
     },
     "response": {
      "$ref": "CommunityPollVote"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    }
   }
  },
  "communityPolls": {
   "methods": {
    "get": {
     "id": "orkut.communityPolls.get",
     "path": "communities/{communityId}/polls/{pollId}",
     "httpMethod": "GET",
     "description": "Retrieves one specific poll of a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community for whose poll will be retrieved.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "pollId": {
       "type": "string",
       "description": "The ID of the poll to get.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "pollId"
     ],
     "response": {
      "$ref": "CommunityPoll"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    },
    "list": {
     "id": "orkut.communityPolls.list",
     "path": "communities/{communityId}/polls",
     "httpMethod": "GET",
     "description": "Retrieves the polls of a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community which polls will be listed.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of polls to include in the response.",
       "format": "uint32",
       "minimum": "1",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token that allows pagination.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "communityId"
     ],
     "response": {
      "$ref": "CommunityPollList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "communityRelated": {
   "methods": {
    "list": {
     "id": "orkut.communityRelated.list",
     "path": "communities/{communityId}/related",
     "httpMethod": "GET",
     "description": "Retrieves the communities related to another one.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community whose related communities will be listed.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "communityId"
     ],
     "response": {
      "$ref": "CommunityList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "communityTopics": {
   "methods": {
    "delete": {
     "id": "orkut.communityTopics.delete",
     "path": "communities/{communityId}/topics/{topicId}",
     "httpMethod": "DELETE",
     "description": "Moves a topic of the community to the trash folder.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community whose topic will be moved to the trash folder.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "topicId": {
       "type": "string",
       "description": "The ID of the topic to be moved to the trash folder.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "topicId"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "get": {
     "id": "orkut.communityTopics.get",
     "path": "communities/{communityId}/topics/{topicId}",
     "httpMethod": "GET",
     "description": "Retrieves a topic of a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community whose topic will be retrieved.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "topicId": {
       "type": "string",
       "description": "The ID of the topic to get.",
       "required": true,
       "format": "int64",
       "location": "path"
      }
     },
     "parameterOrder": [
      "communityId",
      "topicId"
     ],
     "response": {
      "$ref": "CommunityTopic"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    },
    "insert": {
     "id": "orkut.communityTopics.insert",
     "path": "communities/{communityId}/topics",
     "httpMethod": "POST",
     "description": "Adds a topic to a given community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community the topic should be added to.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "isShout": {
       "type": "boolean",
       "description": "Whether this topic is a shout.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "communityId"
     ],
     "request": {
      "$ref": "CommunityTopic"
     },
     "response": {
      "$ref": "CommunityTopic"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    },
    "list": {
     "id": "orkut.communityTopics.list",
     "path": "communities/{communityId}/topics",
     "httpMethod": "GET",
     "description": "Retrieves the topics of a community.",
     "parameters": {
      "communityId": {
       "type": "integer",
       "description": "The ID of the community which topics will be listed.",
       "required": true,
       "format": "int32",
       "location": "path"
      },
      "hl": {
       "type": "string",
       "description": "Specifies the interface language (host language) of your user interface.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "The maximum number of topics to include in the response.",
       "format": "uint32",
       "minimum": "1",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "A continuation token that allows pagination.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "communityId"
     ],
     "response": {
      "$ref": "CommunityTopicList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "counters": {
   "methods": {
    "list": {
     "id": "orkut.counters.list",
     "path": "people/{userId}/counters",
     "httpMethod": "GET",
     "description": "Retrieves the counters of a user.",
     "parameters": {
      "userId": {
       "type": "string",
       "description": "The ID of the user whose counters will be listed. Can be me to refer to caller.",
       "required": true,
       "location": "path"
      }
     },
     "parameterOrder": [
      "userId"
     ],
     "response": {
      "$ref": "Counters"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut",
      "https://www.googleapis.com/auth/orkut.readonly"
     ]
    }
   }
  },
  "scraps": {
   "methods": {
    "insert": {
     "id": "orkut.scraps.insert",
     "path": "activities/scraps",
     "httpMethod": "POST",
     "description": "Creates a new scrap.",
     "request": {
      "$ref": "Activity"
     },
     "response": {
      "$ref": "Activity"
     },
     "scopes": [
      "https://www.googleapis.com/auth/orkut"
     ]
    }
   }
  }
 }
}
