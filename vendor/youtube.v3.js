{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "youtube:v3",
 "name": "youtube",
 "canonicalName": "YouTube",
 "version": "v3",
 "revision": "20120831",
 "title": "YouTube API",
 "description": "Programmatic access to YouTube features.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/youtube-16.png",
  "x32": "http://www.google.com/images/icons/product/youtube-32.png"
 },
 "documentationLink": "https://developers.google.com/youtube",
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/youtube/v3/",
 "basePath": "/youtube/v3/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "youtube/v3/",
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
    "https://www.googleapis.com/auth/youtube": {
     "description": "Manage your YouTube account"
    },
    "https://www.googleapis.com/auth/youtube.readonly": {
     "description": "View your YouTube account"
    },
    "https://www.googleapis.com/auth/youtube.upload": {
     "description": "Manage your YouTube videos"
    },
    "https://www.googleapis.com/auth/youtubepartner": {
     "description": "View and manage your assets and associated content on YouTube"
    }
   }
  }
 },
 "schemas": {
  "Activity": {
   "id": "Activity",
   "type": "object",
   "description": "JSON template for a YouTube activity.",
   "properties": {
    "contentDetails": {
     "$ref": "ActivityContentDetails",
     "description": "Type specific information about the activity."
    },
    "etag": {
     "type": "string",
     "description": "The eTag of the activity."
    },
    "id": {
     "type": "string",
     "description": "The unique ID of the activity."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#activity"
    },
    "snippet": {
     "$ref": "ActivitySnippet",
     "description": "Basic details about the activity: title, description, thumbnails."
    }
   }
  },
  "ActivityContentDetails": {
   "id": "ActivityContentDetails",
   "type": "object",
   "description": "JSON template for the contentDetails part of a activity.",
   "properties": {
    "bulletin": {
     "type": "object",
     "description": "Only present if the type is \"bulletin\".",
     "properties": {
      "resourceId": {
       "$ref": "ResourceId",
       "description": "ID of the resource this bulletin is about."
      }
     }
    },
    "comment": {
     "type": "object",
     "description": "Only present if the type is \"comment\".",
     "properties": {
      "resourceId": {
       "$ref": "ResourceId",
       "description": "ID of the commented resource."
      }
     }
    },
    "favorite": {
     "type": "object",
     "description": "Only present if the type is \"favorite\".",
     "properties": {
      "resourceId": {
       "$ref": "ResourceId",
       "description": "ID of the favorited resource."
      }
     }
    },
    "like": {
     "type": "object",
     "description": "Only present if the type is \"like\".",
     "properties": {
      "resourceId": {
       "$ref": "ResourceId",
       "description": "ID of the rated resource."
      }
     }
    },
    "playlistItem": {
     "type": "object",
     "description": "Only present if the type is \"playlistItem\".",
     "properties": {
      "playlistId": {
       "type": "string",
       "description": "ID of the playlist the video was added to."
      },
      "videoId": {
       "type": "string",
       "description": "ID of the video added to the playlist."
      }
     }
    },
    "recommendation": {
     "type": "object",
     "description": "Only set if the type is \"recommendation\".",
     "properties": {
      "reason": {
       "type": "string",
       "description": "Reason for which the video was recommended."
      },
      "resourceId": {
       "$ref": "ResourceId",
       "description": "ID of the recommended resource."
      },
      "seedResourceId": {
       "$ref": "ResourceId",
       "description": "ID of the video that caused this recommendation."
      }
     }
    },
    "social": {
     "type": "object",
     "description": "Only present if the type is \"social\".",
     "properties": {
      "author": {
       "type": "string",
       "description": "Author of the post."
      },
      "imageUrl": {
       "type": "string",
       "description": "Image of the post author."
      },
      "referenceUrl": {
       "type": "string",
       "description": "Url of the social post."
      },
      "resourceId": {
       "$ref": "ResourceId",
       "description": "ID of the resource this social activity is about."
      },
      "type": {
       "type": "string",
       "description": "Type of the social network."
      }
     }
    },
    "subscription": {
     "type": "object",
     "description": "Only present if the type is \"subscription\".",
     "properties": {
      "resourceId": {
       "$ref": "ResourceId",
       "description": "ID of the resource subscribed to."
      }
     }
    },
    "upload": {
     "type": "object",
     "description": "Only present if the type is \"upload\".",
     "properties": {
      "videoId": {
       "type": "string",
       "description": "ID of the uploaded video."
      }
     }
    }
   }
  },
  "ActivityListResponse": {
   "id": "ActivityListResponse",
   "type": "object",
   "description": "JSON template for a ActivityService.List() response.",
   "properties": {
    "activities": {
     "type": "array",
     "description": "List of activities matching the request criteria.",
     "items": {
      "$ref": "Activity"
     }
    },
    "etag": {
     "type": "string",
     "description": "The eTag of the response."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#activityListResponse"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token to the next page."
    },
    "pageInfo": {
     "$ref": "PageInfo",
     "description": "Paging information for the list result."
    },
    "prevPageToken": {
     "type": "string",
     "description": "Token to the previous page."
    }
   }
  },
  "ActivitySnippet": {
   "id": "ActivitySnippet",
   "type": "object",
   "description": "JSON template for the snippet part of a activity.",
   "properties": {
    "channelId": {
     "type": "string",
     "description": "Channel publishing the activity."
    },
    "description": {
     "type": "string",
     "description": "Description of the activity."
    },
    "groupId": {
     "type": "string",
     "description": "Id of the group that this activity is part of."
    },
    "publishedAt": {
     "type": "string",
     "description": "Date and time the activity was published at.",
     "format": "date-time"
    },
    "thumbnails": {
     "type": "object",
     "description": "Activity thumbnails.",
     "additionalProperties": {
      "$ref": "Thumbnail",
      "description": "A map of thumbnails by their name."
     }
    },
    "title": {
     "type": "string",
     "description": "Title of the activity."
    },
    "type": {
     "type": "string",
     "description": "Type of the activity."
    }
   }
  },
  "Channel": {
   "id": "Channel",
   "type": "object",
   "description": "JSON template for a YouTube Channel.",
   "properties": {
    "contentDetails": {
     "$ref": "ChannelContentDetails",
     "description": "Information about the channel content: upload playlist id, privacy status."
    },
    "etag": {
     "type": "string",
     "description": "The eTag of the channel."
    },
    "id": {
     "type": "string",
     "description": "The unique ID of the channel."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API resource.",
     "default": "youtube#channel"
    },
    "snippet": {
     "$ref": "ChannelSnippet",
     "description": "Basic details about the channel: title, description, and thumbnails."
    },
    "statistics": {
     "$ref": "ChannelStatistics",
     "description": "Statistics about the channel: number of subscribers, views, and comments."
    },
    "topicDetails": {
     "$ref": "ChannelTopicDetails",
     "description": "Information about channel topics"
    }
   }
  },
  "ChannelCategory": {
   "id": "ChannelCategory",
   "type": "object",
   "description": "JSON template for a YouTube guide category.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the guide category."
    },
    "id": {
     "type": "string",
     "description": "The unique ID of the guide category."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API resource.",
     "default": "youtube#channelCategory"
    },
    "snippet": {
     "$ref": "GuideCategorySnippet",
     "description": "Basic details about the category: title."
    }
   }
  },
  "ChannelCategoryListResponse": {
   "id": "ChannelCategoryListResponse",
   "type": "object",
   "description": "JSON template for a GuideCategoryService.List() response.",
   "properties": {
    "channelCategories": {
     "type": "array",
     "description": "List of categories matching the request criteria.",
     "items": {
      "$ref": "ChannelCategory"
     }
    },
    "etag": {
     "type": "string",
     "description": "The eTag of the response."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#channelCategoryListResponse"
    }
   }
  },
  "ChannelContentDetails": {
   "id": "ChannelContentDetails",
   "type": "object",
   "description": "JSON template for the content details part of a channel.",
   "properties": {
    "privacyStatus": {
     "type": "string",
     "description": "Privacy status of the channel."
    },
    "uploads": {
     "type": "string",
     "description": "The ID of the playlist containing the uploads of this channel."
    }
   }
  },
  "ChannelListResponse": {
   "id": "ChannelListResponse",
   "type": "object",
   "description": "JSON template for a ChannelService.List() response.",
   "properties": {
    "channels": {
     "type": "array",
     "description": "List of channels matching the request criteria.",
     "items": {
      "$ref": "Channel"
     }
    },
    "etag": {
     "type": "string",
     "description": "The eTag of the response."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#channelListResponse"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token to the next page."
    },
    "pageInfo": {
     "$ref": "PageInfo",
     "description": "Paging information for the list result."
    },
    "prevPageToken": {
     "type": "string",
     "description": "Token to the previous page."
    }
   }
  },
  "ChannelSnippet": {
   "id": "ChannelSnippet",
   "type": "object",
   "description": "JSON template for the snippet part of a channel.",
   "properties": {
    "channelId": {
     "type": "string",
     "description": "Id of the channel."
    },
    "description": {
     "type": "string",
     "description": "Description of the channel."
    },
    "publishedAt": {
     "type": "string",
     "description": "Date and time the channel was published at.",
     "format": "date-time"
    },
    "thumbnails": {
     "type": "object",
     "description": "Channel thumbnails.",
     "additionalProperties": {
      "$ref": "Thumbnail",
      "description": "A map of thumbnails by their name."
     }
    },
    "title": {
     "type": "string",
     "description": "Title of the channel."
    }
   }
  },
  "ChannelStatistics": {
   "id": "ChannelStatistics",
   "type": "object",
   "description": "JSON template for the statistics part of a channel.",
   "properties": {
    "commentCount": {
     "type": "string",
     "description": "Number of comments for this channel.",
     "format": "uint64"
    },
    "subscriberCount": {
     "type": "string",
     "description": "Number of subscribers to this channel.",
     "format": "uint64"
    },
    "videoCount": {
     "type": "string",
     "description": "Number of videos in the channel.",
     "format": "uint64"
    },
    "viewCount": {
     "type": "string",
     "description": "Number of times the channel has been viewed.",
     "format": "uint64"
    }
   }
  },
  "ChannelTopicDetails": {
   "id": "ChannelTopicDetails",
   "type": "object",
   "description": "JSON template for the topic details part of a channel.",
   "properties": {
    "topics": {
     "type": "array",
     "description": "List of topic ids for this channel *",
     "items": {
      "type": "string"
     }
    }
   }
  },
  "GuideCategorySnippet": {
   "id": "GuideCategorySnippet",
   "type": "object",
   "description": "JSON template for the details part of a guide category.",
   "properties": {
    "channelId": {
     "type": "string",
     "description": "Channel publishing the guide category.",
     "default": "UCBR8-60-B28hp2BmDPdntcQ"
    },
    "title": {
     "type": "string",
     "description": "Title of the guide category."
    }
   }
  },
  "PageInfo": {
   "id": "PageInfo",
   "type": "object",
   "description": "JSON template for a page info.",
   "properties": {
    "resultPerPage": {
     "type": "integer",
     "description": "The number of results to display for each page.",
     "format": "int32"
    },
    "totalResults": {
     "type": "integer",
     "description": "The total number of results.",
     "format": "int32"
    }
   }
  },
  "Playlist": {
   "id": "Playlist",
   "type": "object",
   "description": "JSON template for a YouTube Playlist.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the playlist."
    },
    "id": {
     "type": "string",
     "description": "The unique id of the playlist."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API resource.",
     "default": "youtube#playlist"
    },
    "snippet": {
     "$ref": "PlaylistSnippet",
     "description": "Basic details about the playlist: title, description, thumbnails."
    },
    "status": {
     "$ref": "PlaylistStatus",
     "description": "Status of the playlist: only privacy_status for now."
    }
   }
  },
  "PlaylistItem": {
   "id": "PlaylistItem",
   "type": "object",
   "description": "JSON template for a YouTube Playlist item.",
   "properties": {
    "contentDetails": {
     "$ref": "PlaylistItemContentDetails",
     "description": "Content details about the playlist item: start and end clipping time."
    },
    "etag": {
     "type": "string",
     "description": "The eTag of the playlist item."
    },
    "id": {
     "type": "string",
     "description": "The unique id of the playlist item."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API resource.",
     "default": "youtube#playlistItem"
    },
    "snippet": {
     "$ref": "PlaylistItemSnippet",
     "description": "Basic details about the playlist item: title, description, thumbnails."
    }
   }
  },
  "PlaylistItemContentDetails": {
   "id": "PlaylistItemContentDetails",
   "type": "object",
   "description": "JSON template for the content details part of a playlist item.",
   "properties": {
    "endAt": {
     "type": "string",
     "description": "The time video playback ends."
    },
    "note": {
     "type": "string",
     "description": "The user-generated note for this item."
    },
    "startAt": {
     "type": "string",
     "description": "The time video playback begins."
    },
    "videoId": {
     "type": "string",
     "description": "ID of the video."
    }
   }
  },
  "PlaylistItemListResponse": {
   "id": "PlaylistItemListResponse",
   "type": "object",
   "description": "JSON template for a PlaylistItemService.List() response.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the response."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#playlistItemListResponse"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token to the next page."
    },
    "pageInfo": {
     "$ref": "PageInfo",
     "description": "Paging information for the list result."
    },
    "playlistItems": {
     "type": "array",
     "description": "List of playlist items matching the request criteria.",
     "items": {
      "$ref": "PlaylistItem"
     }
    },
    "prevPageToken": {
     "type": "string",
     "description": "Token to the previous page."
    }
   }
  },
  "PlaylistItemSnippet": {
   "id": "PlaylistItemSnippet",
   "type": "object",
   "description": "JSON template for the snippet part of a playlist item.",
   "properties": {
    "channelId": {
     "type": "string",
     "description": "Channel publishing the playlist item."
    },
    "description": {
     "type": "string",
     "description": "Description of the playlist item."
    },
    "playlistId": {
     "type": "string",
     "description": "The playlist the item is part of."
    },
    "position": {
     "type": "integer",
     "description": "The position of the item within the playlist.",
     "format": "uint32"
    },
    "publishedAt": {
     "type": "string",
     "description": "Date and time the playlist item was published at.",
     "format": "date-time"
    },
    "resourceId": {
     "$ref": "ResourceId",
     "description": "The ID of the resource referenced by the playlist item."
    },
    "thumbnails": {
     "type": "object",
     "description": "Playlist item thumbnails.",
     "additionalProperties": {
      "$ref": "Thumbnail",
      "description": "A map of thumbnails by their name."
     }
    },
    "title": {
     "type": "string",
     "description": "Title of the playlist item."
    }
   }
  },
  "PlaylistListResponse": {
   "id": "PlaylistListResponse",
   "type": "object",
   "description": "JSON template for a PlaylistService.List() response.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the response."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#playlistListResponse"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token to the next page."
    },
    "pageInfo": {
     "$ref": "PageInfo",
     "description": "Paging information for the list result."
    },
    "playlists": {
     "type": "array",
     "description": "List of playlists matching the request criteria.",
     "items": {
      "$ref": "Playlist"
     }
    },
    "prevPageToken": {
     "type": "string",
     "description": "Token to the previous page."
    }
   }
  },
  "PlaylistSnippet": {
   "id": "PlaylistSnippet",
   "type": "object",
   "description": "JSON template for the snippet part of a playlist.",
   "properties": {
    "channelId": {
     "type": "string",
     "description": "Channel publishing the playlist."
    },
    "description": {
     "type": "string",
     "description": "Description of the playlist."
    },
    "publishedAt": {
     "type": "string",
     "description": "Date and time the playlist was published at.",
     "format": "date-time"
    },
    "thumbnails": {
     "type": "object",
     "description": "Playlist thumbnails.",
     "additionalProperties": {
      "$ref": "Thumbnail",
      "description": "A map of thumbnails by their name."
     }
    },
    "title": {
     "type": "string",
     "description": "Title of the playlist."
    }
   }
  },
  "PlaylistStatus": {
   "id": "PlaylistStatus",
   "type": "object",
   "description": "JSON template for the status part of a playlist.",
   "properties": {
    "privacyStatus": {
     "type": "string",
     "description": "Privacy of the playlist."
    }
   }
  },
  "ResourceId": {
   "id": "ResourceId",
   "type": "object",
   "description": "JSON template for a resource id.",
   "properties": {
    "channelId": {
     "type": "string",
     "description": "ID of the referred channel. Present only when type is \"CHANNEL\"."
    },
    "kind": {
     "type": "string",
     "description": "The kind of the referred resource."
    },
    "playlistId": {
     "type": "string",
     "description": "ID of the referred playlist. Present only when type is \"PLAYLIST\"."
    },
    "videoId": {
     "type": "string",
     "description": "ID of the referred video. Present only when type is \"VIDEO\"."
    }
   }
  },
  "SearchListResponse": {
   "id": "SearchListResponse",
   "type": "object",
   "description": "JSON template for a SearchService.List() response.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the response."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#searchListResponse"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token to the next page."
    },
    "pageInfo": {
     "$ref": "PageInfo",
     "description": "Paging information for the search result."
    },
    "prevPageToken": {
     "type": "string",
     "description": "Token to the previous page."
    },
    "searchResults": {
     "type": "array",
     "description": "List of results matching the request criteria.",
     "items": {
      "$ref": "SearchResult"
     }
    }
   }
  },
  "SearchResult": {
   "id": "SearchResult",
   "type": "object",
   "description": "JSON template for a YouTube Search result.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the search result."
    },
    "id": {
     "$ref": "ResourceId",
     "description": "The id of the resource."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API resource.",
     "default": "youtube#searchResult"
    },
    "snippet": {
     "$ref": "SearchResultSnippet",
     "description": "Basic details about the search result: title, description, author."
    }
   }
  },
  "SearchResultSnippet": {
   "id": "SearchResultSnippet",
   "type": "object",
   "description": "JSON template for the snippet part of a search result.",
   "properties": {
    "channelId": {
     "type": "string",
     "description": "Channel publishing the found resource."
    },
    "description": {
     "type": "string",
     "description": "Description of the found resource."
    },
    "publishedAt": {
     "type": "string",
     "description": "Date and time the found resource was published at.",
     "format": "date-time"
    },
    "thumbnails": {
     "type": "object",
     "description": "Thumbnails for the found resource.",
     "additionalProperties": {
      "$ref": "Thumbnail",
      "description": "A map of thumbnails by their name."
     }
    },
    "title": {
     "type": "string",
     "description": "Title of the found resource."
    }
   }
  },
  "Subscription": {
   "id": "Subscription",
   "type": "object",
   "description": "JSON template for a YouTube Subscription.",
   "properties": {
    "contentDetails": {
     "$ref": "SubscriptionStatistics",
     "description": "Basic statistics about the subscription"
    },
    "etag": {
     "type": "string",
     "description": "The eTag of the subscription."
    },
    "id": {
     "type": "string",
     "description": "The unique id of the subscription."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API resource.",
     "default": "youtube#subscription"
    },
    "snippet": {
     "$ref": "SubscriptionSnippet",
     "description": "Basic details about the subscription"
    }
   }
  },
  "SubscriptionListResponse": {
   "id": "SubscriptionListResponse",
   "type": "object",
   "description": "JSON template for a SubscriptionService.List() response.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the response."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#subscriptionListResponse"
    },
    "nextPageToken": {
     "type": "string",
     "description": "Token to the next page."
    },
    "pageInfo": {
     "$ref": "PageInfo",
     "description": "Paging information for the list result."
    },
    "prevPageToken": {
     "type": "string",
     "description": "Token to the previous page."
    },
    "subscriptions": {
     "type": "array",
     "description": "List of subscriptions matching the request criteria.",
     "items": {
      "$ref": "Subscription"
     }
    }
   }
  },
  "SubscriptionSnippet": {
   "id": "SubscriptionSnippet",
   "type": "object",
   "description": "JSON template for the snippet part of a subscription.",
   "properties": {
    "channelId": {
     "type": "string",
     "description": "Channel publishing the subscription."
    },
    "description": {
     "type": "string",
     "description": "Description of the subscription."
    },
    "publishedAt": {
     "type": "string",
     "description": "Date and time the subscription was published at.",
     "format": "date-time"
    },
    "resourceId": {
     "$ref": "ResourceId",
     "description": "The resource subscribed to."
    },
    "thumbnails": {
     "type": "object",
     "description": "Subscription thumbnails.",
     "additionalProperties": {
      "$ref": "Thumbnail",
      "description": "A map of thumbnails by their name."
     }
    },
    "title": {
     "type": "string",
     "description": "Title of the subscription."
    }
   }
  },
  "SubscriptionStatistics": {
   "id": "SubscriptionStatistics",
   "type": "object",
   "description": "JSON template for the contect details part of a subscription.",
   "properties": {
    "countHint": {
     "type": "integer",
     "description": "Approximate number of videos in the channel.",
     "format": "uint32"
    },
    "unreadCount": {
     "type": "integer",
     "description": "Number of unread activities in the channel.",
     "format": "uint32"
    }
   }
  },
  "Thumbnail": {
   "id": "Thumbnail",
   "type": "object",
   "description": "JSON template for a thumbnail.",
   "properties": {
    "url": {
     "type": "string",
     "description": "The URL for the thumbnail."
    }
   }
  },
  "Video": {
   "id": "Video",
   "type": "object",
   "description": "JSON template for a YouTube Video.",
   "properties": {
    "contentDetails": {
     "$ref": "VideoContentDetails",
     "description": "Information about the video content, media file."
    },
    "etag": {
     "type": "string",
     "description": "The eTag of the video."
    },
    "id": {
     "type": "string",
     "description": "The unique id of the video."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API resource.",
     "default": "youtube#video"
    },
    "player": {
     "$ref": "VideoPlayer",
     "description": "Information used to play the video."
    },
    "snippet": {
     "$ref": "VideoSnippet",
     "description": "Basic details about the video: title, description, thumbnails."
    },
    "statistics": {
     "$ref": "VideoStatistics",
     "description": "Statistics about the video: number of views, ratings."
    },
    "status": {
     "$ref": "VideoStatus",
     "description": "Status of the video upload, privacy status."
    }
   }
  },
  "VideoCategory": {
   "id": "VideoCategory",
   "type": "object",
   "description": "JSON template for a YouTube Video Category.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the video."
    },
    "id": {
     "type": "string",
     "description": "The unique id of the video category."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API resource.",
     "default": "youtube#videoCategory"
    },
    "snippet": {
     "$ref": "VideoCategorySnippet",
     "description": "Basic details about the video category."
    }
   }
  },
  "VideoCategoryListResponse": {
   "id": "VideoCategoryListResponse",
   "type": "object",
   "description": "JSON template for a VideoCategoryService.List() response.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the response."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#videoCategoryListResponse"
    },
    "videoCategories": {
     "type": "array",
     "description": "List of video categories matching the request criteria.",
     "items": {
      "$ref": "VideoCategory"
     }
    }
   }
  },
  "VideoCategorySnippet": {
   "id": "VideoCategorySnippet",
   "type": "object",
   "description": "JSON template for the snippet part of a video category.",
   "properties": {
    "channelId": {
     "type": "string",
     "description": "Channel publishing the video category.",
     "default": "UCBR8-60-B28hp2BmDPdntcQ"
    },
    "title": {
     "type": "string",
     "description": "Title of the video category."
    }
   }
  },
  "VideoContentDetails": {
   "id": "VideoContentDetails",
   "type": "object",
   "description": "JSON template for the content details part of a video.",
   "properties": {
    "aspectRatio": {
     "type": "string",
     "description": "The aspect ratio of the video."
    },
    "duration": {
     "type": "string",
     "description": "Duration of the video."
    }
   }
  },
  "VideoListResponse": {
   "id": "VideoListResponse",
   "type": "object",
   "description": "JSON template for a VideoService.List() response.",
   "properties": {
    "etag": {
     "type": "string",
     "description": "The eTag of the response."
    },
    "kind": {
     "type": "string",
     "description": "The type of this API response.",
     "default": "youtube#videoListResponse"
    },
    "videos": {
     "type": "array",
     "description": "List of videos matching the request criteria.",
     "items": {
      "$ref": "Video"
     }
    }
   }
  },
  "VideoPlayer": {
   "id": "VideoPlayer",
   "type": "object",
   "description": "JSON template for the player part of a video.",
   "properties": {
    "embedHtml": {
     "type": "string",
     "description": "Iframe embed for the video."
    }
   }
  },
  "VideoSnippet": {
   "id": "VideoSnippet",
   "type": "object",
   "description": "JSON template for the snippet part of a video.",
   "properties": {
    "categoryId": {
     "type": "string",
     "description": "Video category the video belongs to."
    },
    "channelId": {
     "type": "string",
     "description": "Channel publishing the video."
    },
    "description": {
     "type": "string",
     "description": "Description of the video."
    },
    "publishedAt": {
     "type": "string",
     "description": "Date and time the video was published at.",
     "format": "date-time"
    },
    "tags": {
     "type": "array",
     "description": "Textual tags associated with the video.",
     "items": {
      "type": "string"
     }
    },
    "thumbnails": {
     "type": "object",
     "description": "Video thumbnails.",
     "additionalProperties": {
      "$ref": "Thumbnail",
      "description": "A map of thumbnails by their name."
     }
    },
    "title": {
     "type": "string",
     "description": "Title of the video."
    }
   }
  },
  "VideoStatistics": {
   "id": "VideoStatistics",
   "type": "object",
   "description": "JSON template for the statistics part of a video.",
   "properties": {
    "commentCount": {
     "type": "string",
     "description": "Number of comments for this video.",
     "format": "uint64"
    },
    "dislikeCount": {
     "type": "string",
     "description": "Number of times the video was disliked.",
     "format": "uint64"
    },
    "favoriteCount": {
     "type": "string",
     "description": "Number of times the video was added to a user's favorites list.",
     "format": "uint64"
    },
    "likeCount": {
     "type": "string",
     "description": "Number of times the video was liked.",
     "format": "uint64"
    },
    "viewCount": {
     "type": "string",
     "description": "Number of times the video was viewed.",
     "format": "uint64"
    }
   }
  },
  "VideoStatus": {
   "id": "VideoStatus",
   "type": "object",
   "description": "JSON template for the status part of a video.",
   "properties": {
    "failureReason": {
     "type": "string",
     "description": "Present only if the uploadStatus indicates a failed upload."
    },
    "privacyStatus": {
     "type": "string",
     "description": "Privacy of the video."
    },
    "rejectionReason": {
     "type": "string",
     "description": "Present only if the uploadStatus indicates a rejected upload."
    },
    "uploadStatus": {
     "type": "string",
     "description": "Status of the video upload."
    }
   }
  }
 },
 "resources": {
  "activities": {
   "methods": {
    "insert": {
     "id": "youtube.activities.insert",
     "path": "activities",
     "httpMethod": "POST",
     "description": "Post a channel bulletin.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "request": {
      "$ref": "Activity"
     },
     "response": {
      "$ref": "Activity"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube"
     ]
    },
    "list": {
     "id": "youtube.activities.list",
     "path": "activities",
     "httpMethod": "GET",
     "description": "Browse the YouTube channel activity collection.",
     "parameters": {
      "channelId": {
       "type": "string",
       "description": "YouTube ID of the channel.",
       "location": "query"
      },
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "home": {
       "type": "string",
       "description": "Flag indicating to return user's homepage feed.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "default": "5",
       "format": "uint32",
       "minimum": "0",
       "maximum": "50",
       "location": "query"
      },
      "mine": {
       "type": "string",
       "description": "Flag indicating to return user's activities.",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Token for the page selection.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      },
      "publishedAfter": {
       "type": "string",
       "description": "Only return activities published after given date (inclusive).",
       "format": "date-time",
       "location": "query"
      },
      "publishedBefore": {
       "type": "string",
       "description": "Only return activities published before given date (exclusive).",
       "format": "date-time",
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "response": {
      "$ref": "ActivityListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtube.readonly"
     ]
    }
   }
  },
  "channelCategories": {
   "methods": {
    "list": {
     "id": "youtube.channelCategories.list",
     "path": "channelCategories",
     "httpMethod": "GET",
     "description": "Browse the YouTube guide category collection.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "hl": {
       "type": "string",
       "description": "Language for the returned channelCategories.",
       "default": "en-US",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "Comma-separated YouTube IDs of the channelCategories to be returned.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      },
      "regionCode": {
       "type": "string",
       "description": "Return the channelCategories in the given region code.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "response": {
      "$ref": "ChannelCategoryListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtube.readonly",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    }
   }
  },
  "channels": {
   "methods": {
    "list": {
     "id": "youtube.channels.list",
     "path": "channels",
     "httpMethod": "GET",
     "description": "Browse the YouTube channel collection. Either the 'id' or 'mine' parameter must be set.",
     "parameters": {
      "categoryId": {
       "type": "string",
       "description": "Filter to retrieve the channels within the given category ID.",
       "location": "query"
      },
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "YouTube IDs of the channels to be returned.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "default": "5",
       "format": "uint32",
       "minimum": "0",
       "maximum": "50",
       "location": "query"
      },
      "mine": {
       "type": "string",
       "description": "Filter to only channels owned by authenticated user.",
       "location": "query"
      },
      "mySubscribers": {
       "type": "string",
       "description": "Filter to channels that subscribed to the channel of the authenticated user.",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Token for the page selection.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "response": {
      "$ref": "ChannelListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtube.readonly",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    }
   }
  },
  "playlistItems": {
   "methods": {
    "delete": {
     "id": "youtube.playlistItems.delete",
     "path": "playlistItems",
     "httpMethod": "DELETE",
     "description": "Deletes playlist items by IDs.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "YouTube IDs of the playlist items to be deleted.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "insert": {
     "id": "youtube.playlistItems.insert",
     "path": "playlistItems",
     "httpMethod": "POST",
     "description": "Insert a resource into a playlist.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "request": {
      "$ref": "PlaylistItem"
     },
     "response": {
      "$ref": "PlaylistItem"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "list": {
     "id": "youtube.playlistItems.list",
     "path": "playlistItems",
     "httpMethod": "GET",
     "description": "Browse the YouTube playlist collection.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "YouTube IDs of the playlist items to be returned.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "default": "5",
       "format": "uint32",
       "minimum": "0",
       "maximum": "50",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Token for the page selection.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      },
      "playlistId": {
       "type": "string",
       "description": "Retrieves playlist items from the given playlist id.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "response": {
      "$ref": "PlaylistItemListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtube.readonly",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "update": {
     "id": "youtube.playlistItems.update",
     "path": "playlistItems",
     "httpMethod": "PUT",
     "description": "Update a playlist item.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "request": {
      "$ref": "PlaylistItem"
     },
     "response": {
      "$ref": "PlaylistItem"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    }
   }
  },
  "playlists": {
   "methods": {
    "delete": {
     "id": "youtube.playlists.delete",
     "path": "playlists",
     "httpMethod": "DELETE",
     "description": "Deletes playlists by IDs.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "YouTube IDs of the playlists to be deleted.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "insert": {
     "id": "youtube.playlists.insert",
     "path": "playlists",
     "httpMethod": "POST",
     "description": "Create a playlist.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "request": {
      "$ref": "Playlist"
     },
     "response": {
      "$ref": "Playlist"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "list": {
     "id": "youtube.playlists.list",
     "path": "playlists",
     "httpMethod": "GET",
     "description": "Browse the YouTube playlist collection.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "Comma-separated YouTube IDs of the playlists to be returned.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of results to return",
       "default": "5",
       "format": "uint32",
       "minimum": "0",
       "maximum": "50",
       "location": "query"
      },
      "mine": {
       "type": "string",
       "description": "Flag indicating only return the playlists of the authenticated user.",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Token for the page selection.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "response": {
      "$ref": "PlaylistListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtube.readonly",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "update": {
     "id": "youtube.playlists.update",
     "path": "playlists",
     "httpMethod": "PUT",
     "description": "Update a playlist.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "request": {
      "$ref": "Playlist"
     },
     "response": {
      "$ref": "Playlist"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    }
   }
  },
  "search": {
   "methods": {
    "list": {
     "id": "youtube.search.list",
     "path": "search",
     "httpMethod": "GET",
     "description": "Universal search for youtube.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of search results to return per page.",
       "default": "5",
       "format": "uint32",
       "minimum": "0",
       "maximum": "50",
       "location": "query"
      },
      "order": {
       "type": "string",
       "description": "Sort order.",
       "default": "relevance",
       "enum": [
        "date",
        "rating",
        "relevance",
        "view_count"
       ],
       "enumDescriptions": [
        "Sort according to the date.",
        "Sort according to the rating.",
        "Sort according to the relevance.",
        "Sort according to the view count."
       ],
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Token for the page selection.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      },
      "published": {
       "type": "string",
       "description": "Only search for resources uploaded at a specific pediod",
       "enum": [
        "any",
        "thisWeek",
        "today"
       ],
       "enumDescriptions": [
        "No filter on the release date",
        "Videos uploaded this month",
        "Videos uploaded today"
       ],
       "location": "query"
      },
      "q": {
       "type": "string",
       "description": "Query to search in Youtube.",
       "location": "query"
      },
      "relatedToVideo": {
       "type": "string",
       "description": "Search for resources related to this video. Need to be used with type set to 'video'",
       "location": "query"
      },
      "topicId": {
       "type": "string",
       "description": "Only search for resources with the specified topic",
       "location": "query"
      },
      "type": {
       "type": "string",
       "description": "Type of resource to search.",
       "enum": [
        "channel",
        "playlist",
        "video"
       ],
       "enumDescriptions": [
        "Search for channels.",
        "Search for playlists.",
        "Search for videos."
       ],
       "repeated": true,
       "location": "query"
      },
      "videoCaption": {
       "type": "string",
       "description": "Add a filter on the the presence of captions on the videos.",
       "enum": [
        "any",
        "closedCaption",
        "none"
       ],
       "enumDescriptions": [
        "No filter on the captions.",
        "Videos with closed captions.",
        "Videos without captions."
       ],
       "location": "query"
      },
      "videoDefinition": {
       "type": "string",
       "description": "Add a filter for the definition of the videos.",
       "enum": [
        "any",
        "high",
        "standard"
       ],
       "enumDescriptions": [
        "No filter on the definition.",
        "Videos in high definition.",
        "Videos in standard definition."
       ],
       "location": "query"
      },
      "videoDimension": {
       "type": "string",
       "description": "Add a filter for the number of dimensions in the videos.",
       "enum": [
        "2d",
        "3d",
        "any"
       ],
       "enumDescriptions": [
        "Videos in two dimensions.",
        "Videos in three dimensions.",
        "No filter on the dimension."
       ],
       "location": "query"
      },
      "videoDuration": {
       "type": "string",
       "description": "Add a filter on the duration of the videos.",
       "enum": [
        "any",
        "long",
        "medium",
        "short"
       ],
       "enumDescriptions": [
        "No filter on the duration.",
        "Videos with a duration longer than 20 minutes.",
        "Videos with a duration between 4 and 20 minutes.",
        "Videos with a duration under 4 minutes."
       ],
       "location": "query"
      },
      "videoLicense": {
       "type": "string",
       "description": "Add a filter on the licensing of the videos.",
       "enum": [
        "any",
        "creativeCommon",
        "youtube"
       ],
       "enumDescriptions": [
        "No filter on the license.",
        "Videos under the Creative Common license.",
        "Videos under the YouTube license."
       ],
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "response": {
      "$ref": "SearchListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtube.readonly",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    }
   }
  },
  "subscription": {
   "methods": {
    "delete": {
     "id": "youtube.subscription.delete",
     "path": "subscriptions",
     "httpMethod": "DELETE",
     "description": "Deletes subscriptions by IDs.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "YouTube IDs of the subscription to be deleted.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "insert": {
     "id": "youtube.subscription.insert",
     "path": "subscriptions",
     "httpMethod": "POST",
     "description": "Insert a subscription.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "request": {
      "$ref": "Subscription"
     },
     "response": {
      "$ref": "Subscription"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "list": {
     "id": "youtube.subscription.list",
     "path": "subscriptions",
     "httpMethod": "GET",
     "description": "Browse user's subscription collection.",
     "parameters": {
      "channelId": {
       "type": "string",
       "description": "Only return subscriptions to given channelId.",
       "location": "query"
      },
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "forChannelId": {
       "type": "string",
       "description": "Takes a comma separated list of channel IDs. Filters the returned list to only those matching these channels",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "YouTube IDs of the subscriptions to be returned.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Maximum number of search results to return per page.",
       "default": "5",
       "format": "uint32",
       "minimum": "0",
       "maximum": "50",
       "location": "query"
      },
      "mine": {
       "type": "string",
       "description": "Flag indicating only return the subscriptions of the authenticated user.",
       "location": "query"
      },
      "order": {
       "type": "string",
       "description": "Sort order.",
       "default": "SUBSCRIPTION_ORDER_RELEVANCE",
       "enum": [
        "alphabetical",
        "relevance",
        "unread"
       ],
       "enumDescriptions": [
        "Sort alphabetically",
        "Sort by relevance.",
        "Sort by order of activity."
       ],
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Token for the page selection.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "response": {
      "$ref": "SubscriptionListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    }
   }
  },
  "videocategory": {
   "methods": {
    "list": {
     "id": "youtube.videocategory.list",
     "path": "videoCategories",
     "httpMethod": "GET",
     "description": "Browse list of video categories.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "hl": {
       "type": "string",
       "description": "Language used for the title of the categories.",
       "default": "en_US",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "IDs of the categories to be returned.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      },
      "regionCode": {
       "type": "string",
       "description": "Return all the categories in this region.",
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "response": {
      "$ref": "VideoCategoryListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtube.readonly",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    }
   }
  },
  "videos": {
   "methods": {
    "delete": {
     "id": "youtube.videos.delete",
     "path": "videos",
     "httpMethod": "DELETE",
     "description": "Delete a YouTube video.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "YouTube ID of the video to be deleted.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "id"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "insert": {
     "id": "youtube.videos.insert",
     "path": "videos",
     "httpMethod": "POST",
     "description": "Upload a video to YouTube.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "request": {
      "$ref": "Video"
     },
     "response": {
      "$ref": "Video"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtube.upload"
     ],
     "supportsMediaUpload": true,
     "mediaUpload": {
      "accept": [
       "application/octet-stream",
       "video/*"
      ],
      "maxSize": "64GB",
      "protocols": {
       "simple": {
        "multipart": true,
        "path": "/upload/youtube/v3/videos"
       },
       "resumable": {
        "multipart": true,
        "path": "/resumable/upload/youtube/v3/videos"
       }
      }
     }
    },
    "list": {
     "id": "youtube.videos.list",
     "path": "videos",
     "httpMethod": "GET",
     "description": "Browse the YouTube video collection.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "id": {
       "type": "string",
       "description": "YouTube IDs of the videos to be returned.",
       "required": true,
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "id",
      "part"
     ],
     "response": {
      "$ref": "VideoListResponse"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtube.readonly",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    },
    "update": {
     "id": "youtube.videos.update",
     "path": "videos",
     "httpMethod": "PUT",
     "description": "Update a video.",
     "parameters": {
      "contentOwnerId": {
       "type": "string",
       "description": "The authenticated user acts on behalf of this content owner.",
       "location": "query"
      },
      "part": {
       "type": "string",
       "description": "One or more parts to return on the current request.",
       "required": true,
       "location": "query"
      }
     },
     "parameterOrder": [
      "part"
     ],
     "request": {
      "$ref": "Video"
     },
     "response": {
      "$ref": "Video"
     },
     "scopes": [
      "https://www.googleapis.com/auth/youtube",
      "https://www.googleapis.com/auth/youtubepartner"
     ]
    }
   }
  }
 }
}
