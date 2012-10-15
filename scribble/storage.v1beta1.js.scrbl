#lang scribble/manual
@title{Cloud Storage API v1beta1}
Lets you store and retrieve potentially-large, immutable data objects.
@hyperlink["https://developers.google.com/storage/docs/json_api/" "Documentation link"]
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


@section{Functions for the `buckets' resource}
@defproc[(storage.buckets.list
[projectId string?]
[#:projection projection string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:max-results max-results string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of buckets for a given project.

@racket[projectId]: A valid API project identifier.

@racket[projection]: Set of properties to return. Defaults to no_acl.

@racket[pageToken]: A previously-returned page token representing part of the larger set of results to view.

@racket[max-results]: Maximum number of buckets to return.

}

@defproc[(storage.buckets.get
[bucket string?]
[#:projection projection string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns metadata for the specified bucket.

@racket[bucket]: Name of a bucket.

@racket[projection]: Set of properties to return. Defaults to no_acl.

}

@defproc[(storage.buckets.insert
[#:projection projection string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:location location string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:owner owner string? 'N/A]
[#:acl acl string? 'N/A]
[#:defaultObjectAcl defaultObjectAcl string? 'N/A]
[#:projectId projectId string? 'N/A]
[#:timeCreated timeCreated string? 'N/A]
[#:website website string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new bucket.

@racket[projection]: Set of properties to return. Defaults to no_acl, unless the bucket resource specifies acl or defaultObjectAcl properties, when it defaults to full.

@racket[id]: The name of the bucket.

@racket[kind]: The kind of item this is. For buckets, this is always storage#bucket.

@racket[location]: The location of the bucket. Object data for objects in the bucket resides in physical storage in this location. Can be US or EU. Defaults to US.

@racket[selfLink]: The URI of this bucket.

@racket[owner]: The owner of the bucket. This will always be the project team's owner group.

@racket[acl]: Access controls on the bucket.

@racket[defaultObjectAcl]: Default access controls to apply to new objects when no ACL is provided.

@racket[projectId]: The project the bucket belongs to.

@racket[timeCreated]: Creation time of the bucket in RFC 3339 format.

@racket[website]: The bucket's website configuration.

}

@defproc[(storage.buckets.patch
[bucket string?]
[#:projection projection string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:location location string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:owner owner string? 'N/A]
[#:acl acl string? 'N/A]
[#:defaultObjectAcl defaultObjectAcl string? 'N/A]
[#:projectId projectId string? 'N/A]
[#:timeCreated timeCreated string? 'N/A]
[#:website website string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a bucket. This method supports patch semantics.

@racket[bucket]: Name of a bucket.

@racket[projection]: Set of properties to return. Defaults to full.

@racket[id]: The name of the bucket.

@racket[kind]: The kind of item this is. For buckets, this is always storage#bucket.

@racket[location]: The location of the bucket. Object data for objects in the bucket resides in physical storage in this location. Can be US or EU. Defaults to US.

@racket[selfLink]: The URI of this bucket.

@racket[owner]: The owner of the bucket. This will always be the project team's owner group.

@racket[acl]: Access controls on the bucket.

@racket[defaultObjectAcl]: Default access controls to apply to new objects when no ACL is provided.

@racket[projectId]: The project the bucket belongs to.

@racket[timeCreated]: Creation time of the bucket in RFC 3339 format.

@racket[website]: The bucket's website configuration.

}

@defproc[(storage.buckets.update
[bucket string?]
[#:projection projection string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:location location string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:owner owner string? 'N/A]
[#:acl acl string? 'N/A]
[#:defaultObjectAcl defaultObjectAcl string? 'N/A]
[#:projectId projectId string? 'N/A]
[#:timeCreated timeCreated string? 'N/A]
[#:website website string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a bucket.

@racket[bucket]: Name of a bucket.

@racket[projection]: Set of properties to return. Defaults to full.

@racket[id]: The name of the bucket.

@racket[kind]: The kind of item this is. For buckets, this is always storage#bucket.

@racket[location]: The location of the bucket. Object data for objects in the bucket resides in physical storage in this location. Can be US or EU. Defaults to US.

@racket[selfLink]: The URI of this bucket.

@racket[owner]: The owner of the bucket. This will always be the project team's owner group.

@racket[acl]: Access controls on the bucket.

@racket[defaultObjectAcl]: Default access controls to apply to new objects when no ACL is provided.

@racket[projectId]: The project the bucket belongs to.

@racket[timeCreated]: Creation time of the bucket in RFC 3339 format.

@racket[website]: The bucket's website configuration.

}

@defproc[(storage.buckets.delete
[bucket string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes an empty bucket.

@racket[bucket]: Name of a bucket.

}

@section{Functions for the `bucketAccessControls' resource}
@defproc[(storage.bucketAccessControls.list
[bucket string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves ACL entries on the specified bucket.

@racket[bucket]: Name of a bucket.

}

@defproc[(storage.bucketAccessControls.get
[entity string?]
[bucket string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the ACL entry for the specified entity on the specified bucket.

@racket[entity]: The entity holding the permission. Can be user-userId, group-groupId, allUsers, or allAuthenticatedUsers.

@racket[bucket]: Name of a bucket.

}

@defproc[(storage.bucketAccessControls.insert
[bucket string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:email email string? 'N/A]
[#:domain domain string? 'N/A]
[#:role role string? 'N/A]
[#:entity entity string? 'N/A]
[#:entityId entityId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new ACL entry on the specified bucket.

@racket[bucket]: Name of a bucket.

@racket[id]: The ID of the access-control entry.

@racket[kind]: The kind of item this is. For bucket access control entries, this is always storage#bucketAccessControl.

@racket[selfLink]: The link to this access-control entry.

@racket[email]: The email address associated with the entity, if any.

@racket[domain]: The domain associated with the entity, if any.

@racket[role]: The access permission for the entity. Can be READER, WRITER, or OWNER.

@racket[entity]: The entity holding the permission, in one of the following forms: 
- user-userId 
- user-email 
- group-groupId 
- group-email 
- allUsers 
- allAuthenticatedUsers

@racket[entityId]: The ID for the entity, if any.

}

@defproc[(storage.bucketAccessControls.patch
[entity string?]
[bucket string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:email email string? 'N/A]
[#:domain domain string? 'N/A]
[#:role role string? 'N/A]
[#:entityId entityId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an ACL entry on the specified bucket. This method supports patch semantics.

@racket[entity]: The entity holding the permission. Can be user-userId, group-groupId, allUsers, or allAuthenticatedUsers.

@racket[bucket]: Name of a bucket.

@racket[id]: The ID of the access-control entry.

@racket[kind]: The kind of item this is. For bucket access control entries, this is always storage#bucketAccessControl.

@racket[selfLink]: The link to this access-control entry.

@racket[email]: The email address associated with the entity, if any.

@racket[domain]: The domain associated with the entity, if any.

@racket[role]: The access permission for the entity. Can be READER, WRITER, or OWNER.

@racket[entityId]: The ID for the entity, if any.

}

@defproc[(storage.bucketAccessControls.update
[entity string?]
[bucket string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:email email string? 'N/A]
[#:domain domain string? 'N/A]
[#:role role string? 'N/A]
[#:entityId entityId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an ACL entry on the specified bucket.

@racket[entity]: The entity holding the permission. Can be user-userId, group-groupId, allUsers, or allAuthenticatedUsers.

@racket[bucket]: Name of a bucket.

@racket[id]: The ID of the access-control entry.

@racket[kind]: The kind of item this is. For bucket access control entries, this is always storage#bucketAccessControl.

@racket[selfLink]: The link to this access-control entry.

@racket[email]: The email address associated with the entity, if any.

@racket[domain]: The domain associated with the entity, if any.

@racket[role]: The access permission for the entity. Can be READER, WRITER, or OWNER.

@racket[entityId]: The ID for the entity, if any.

}

@defproc[(storage.bucketAccessControls.delete
[entity string?]
[bucket string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the ACL entry for the specified entity on the specified bucket.

@racket[entity]: The entity holding the permission. Can be user-userId, group-groupId, allUsers, or allAuthenticatedUsers.

@racket[bucket]: Name of a bucket.

}

@section{Functions for the `objectAccessControls' resource}
@defproc[(storage.objectAccessControls.list
[object string?]
[bucket string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves ACL entries on the specified object.

@racket[object]: Name of the object.

@racket[bucket]: Name of a bucket.

}

@defproc[(storage.objectAccessControls.get
[object string?]
[entity string?]
[bucket string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the ACL entry for the specified entity on the specified object.

@racket[object]: Name of the object.

@racket[entity]: The entity holding the permission. Can be user-userId, group-groupId, allUsers, or allAuthenticatedUsers.

@racket[bucket]: Name of a bucket.

}

@defproc[(storage.objectAccessControls.insert
[object string?]
[bucket string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:email email string? 'N/A]
[#:domain domain string? 'N/A]
[#:role role string? 'N/A]
[#:entity entity string? 'N/A]
[#:entityId entityId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a new ACL entry on the specified object.

@racket[object]: Name of the object.

@racket[bucket]: Name of a bucket.

@racket[id]: The ID of the access-control entry.

@racket[kind]: The kind of item this is. For object access control entries, this is always storage#objectAccessControl.

@racket[selfLink]: The link to this access-control entry.

@racket[email]: The email address associated with the entity, if any.

@racket[domain]: The domain associated with the entity, if any.

@racket[role]: The access permission for the entity. Can be READER or OWNER.

@racket[entity]: The entity holding the permission, in one of the following forms: 
- user-userId 
- user-email 
- group-groupId 
- group-email 
- allUsers 
- allAuthenticatedUsers

@racket[entityId]: The ID for the entity, if any.

}

@defproc[(storage.objectAccessControls.patch
[object string?]
[entity string?]
[bucket string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:email email string? 'N/A]
[#:domain domain string? 'N/A]
[#:role role string? 'N/A]
[#:entityId entityId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an ACL entry on the specified object. This method supports patch semantics.

@racket[object]: Name of the object.

@racket[entity]: The entity holding the permission. Can be user-userId, group-groupId, allUsers, or allAuthenticatedUsers.

@racket[bucket]: Name of a bucket.

@racket[id]: The ID of the access-control entry.

@racket[kind]: The kind of item this is. For object access control entries, this is always storage#objectAccessControl.

@racket[selfLink]: The link to this access-control entry.

@racket[email]: The email address associated with the entity, if any.

@racket[domain]: The domain associated with the entity, if any.

@racket[role]: The access permission for the entity. Can be READER or OWNER.

@racket[entityId]: The ID for the entity, if any.

}

@defproc[(storage.objectAccessControls.update
[object string?]
[entity string?]
[bucket string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:email email string? 'N/A]
[#:domain domain string? 'N/A]
[#:role role string? 'N/A]
[#:entityId entityId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates an ACL entry on the specified object.

@racket[object]: Name of the object.

@racket[entity]: The entity holding the permission. Can be user-userId, group-groupId, allUsers, or allAuthenticatedUsers.

@racket[bucket]: Name of a bucket.

@racket[id]: The ID of the access-control entry.

@racket[kind]: The kind of item this is. For object access control entries, this is always storage#objectAccessControl.

@racket[selfLink]: The link to this access-control entry.

@racket[email]: The email address associated with the entity, if any.

@racket[domain]: The domain associated with the entity, if any.

@racket[role]: The access permission for the entity. Can be READER or OWNER.

@racket[entityId]: The ID for the entity, if any.

}

@defproc[(storage.objectAccessControls.delete
[object string?]
[entity string?]
[bucket string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the ACL entry for the specified entity on the specified object.

@racket[object]: Name of the object.

@racket[entity]: The entity holding the permission. Can be user-userId, group-groupId, allUsers, or allAuthenticatedUsers.

@racket[bucket]: Name of a bucket.

}

@section{Functions for the `objects' resource}
@defproc[(storage.objects.list
[bucket string?]
[#:projection projection string? 'N/A]
[#:pageToken pageToken string? 'N/A]
[#:max-results max-results string? 'N/A]
[#:delimiter delimiter string? 'N/A]
[#:prefix prefix string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves a list of objects matching the criteria.

@racket[bucket]: Name of the bucket in which to look for objects.

@racket[projection]: Set of properties to return. Defaults to no_acl.

@racket[pageToken]: A previously-returned page token representing part of the larger set of results to view.

@racket[max-results]: Maximum number of items plus prefixes to return. As duplicate prefixes are omitted, fewer total results may be returned than requested.

@racket[delimiter]: Returns results in a directory-like mode. items will contain only objects whose names, aside from the prefix, do not contain delimiter. Objects whose names, aside from the prefix, contain delimiter will have their name, truncated after the delimiter, returned in prefixes. Duplicate prefixes are omitted.

@racket[prefix]: Filter results to objects whose names begin with this prefix.

}

@defproc[(storage.objects.get
[object string?]
[bucket string?]
[#:projection projection string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves objects or their associated metadata.

@racket[object]: Name of the object.

@racket[bucket]: Name of the bucket in which the object resides.

@racket[projection]: Set of properties to return. Defaults to no_acl.

}

@defproc[(storage.objects.insert
[bucket string?]
[#:name name string? 'N/A]
[#:projection projection string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:metadata metadata string? 'N/A]
[#:owner owner string? 'N/A]
[#:acl acl string? 'N/A]
[#:cacheControl cacheControl string? 'N/A]
[#:contentDisposition contentDisposition string? 'N/A]
[#:contentEncoding contentEncoding string? 'N/A]
[#:contentLanguage contentLanguage string? 'N/A]
[#:media media string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Stores new data blobs and associated metadata.

@racket[bucket]: Name of the bucket in which to store the new object. Overrides the provided object metadata's bucket value, if any.

@racket[name]: Name of the object. Required when the object metadata is not otherwise provided. Overrides the object metadata's name value, if any.

@racket[projection]: Set of properties to return. Defaults to no_acl, unless the object resource specifies the acl property, when it defaults to full.

@racket[id]: The ID of the object.

@racket[kind]: The kind of item this is. For objects, this is always storage#object.

@racket[name]: The name of this object. Required if not specified by URL parameter.

@racket[selfLink]: The link to this object.

@racket[metadata]: User-provided metadata, in key/value pairs.

@racket[owner]: The owner of the object. This will always be the uploader of the object.

@racket[acl]: Access controls on the object.

@racket[cacheControl]: Cache-Control directive for the object data.

@racket[contentDisposition]: Content-Disposition of the object data.

@racket[contentEncoding]: Content-Encoding of the object data.

@racket[contentLanguage]: Content-Language of the object data.

@racket[media]: Object media data. Provided on your behalf when uploading raw media or multipart/related with an auxiliary media part.

}

@defproc[(storage.objects.patch
[object string?]
[bucket string?]
[#:projection projection string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:metadata metadata string? 'N/A]
[#:owner owner string? 'N/A]
[#:acl acl string? 'N/A]
[#:cacheControl cacheControl string? 'N/A]
[#:contentDisposition contentDisposition string? 'N/A]
[#:contentEncoding contentEncoding string? 'N/A]
[#:contentLanguage contentLanguage string? 'N/A]
[#:media media string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a data blob's associated metadata. This method supports patch semantics.

@racket[object]: Name of the object.

@racket[bucket]: Name of the bucket in which the object resides.

@racket[projection]: Set of properties to return. Defaults to full.

@racket[id]: The ID of the object.

@racket[kind]: The kind of item this is. For objects, this is always storage#object.

@racket[name]: The name of this object. Required if not specified by URL parameter.

@racket[selfLink]: The link to this object.

@racket[metadata]: User-provided metadata, in key/value pairs.

@racket[owner]: The owner of the object. This will always be the uploader of the object.

@racket[acl]: Access controls on the object.

@racket[cacheControl]: Cache-Control directive for the object data.

@racket[contentDisposition]: Content-Disposition of the object data.

@racket[contentEncoding]: Content-Encoding of the object data.

@racket[contentLanguage]: Content-Language of the object data.

@racket[media]: Object media data. Provided on your behalf when uploading raw media or multipart/related with an auxiliary media part.

}

@defproc[(storage.objects.update
[object string?]
[bucket string?]
[#:projection projection string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:metadata metadata string? 'N/A]
[#:owner owner string? 'N/A]
[#:acl acl string? 'N/A]
[#:cacheControl cacheControl string? 'N/A]
[#:contentDisposition contentDisposition string? 'N/A]
[#:contentEncoding contentEncoding string? 'N/A]
[#:contentLanguage contentLanguage string? 'N/A]
[#:media media string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates a data blob's associated metadata.

@racket[object]: Name of the object.

@racket[bucket]: Name of the bucket in which the object resides.

@racket[projection]: Set of properties to return. Defaults to full.

@racket[id]: The ID of the object.

@racket[kind]: The kind of item this is. For objects, this is always storage#object.

@racket[name]: The name of this object. Required if not specified by URL parameter.

@racket[selfLink]: The link to this object.

@racket[metadata]: User-provided metadata, in key/value pairs.

@racket[owner]: The owner of the object. This will always be the uploader of the object.

@racket[acl]: Access controls on the object.

@racket[cacheControl]: Cache-Control directive for the object data.

@racket[contentDisposition]: Content-Disposition of the object data.

@racket[contentEncoding]: Content-Encoding of the object data.

@racket[contentLanguage]: Content-Language of the object data.

@racket[media]: Object media data. Provided on your behalf when uploading raw media or multipart/related with an auxiliary media part.

}

@defproc[(storage.objects.delete
[object string?]
[bucket string?]
[#:fields fields string? 'N/A]
[#:key key string? 'N/A]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes data blobs and associated metadata.

@racket[object]: Name of the object.

@racket[bucket]: Name of the bucket in which the object resides.

}

