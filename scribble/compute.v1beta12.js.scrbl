#lang scribble/manual
@(require planet/scribble (for-label racket))

@title{Compute Engine API v1beta12}
@margin-note{This documentation has been automatically generated using information supplied by the Google API Discovery service.}
API for the Google Compute Engine service.
@hyperlink["https://developers.google.com/compute/docs/reference/v1beta12" "Google documentation."]
@table-of-contents{}
@defmodule[gapi/macro]
@racket[(require-gapi-doc "compute.v1beta12.js")]
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

@subsection{instances}
@defproc[(compute-instances-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of instance resources contained within the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-instances-get
[#:project project string?]
[#:instance instance string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified instance resource.

@racket[project]: Name of the project scoping this request.

@racket[instance]: Name of the instance resource to return.

}

@defproc[(compute-instances-insert
[#:project project string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:image image string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:status status string? 'N/A]
[#:creationTimestamp creationTimestamp string? 'N/A]
[#:zone zone string? 'N/A]
[#:disks disks string? 'N/A]
[#:machineType machineType string? 'N/A]
[#:metadata metadata string? 'N/A]
[#:networkInterfaces networkInterfaces string? 'N/A]
[#:serviceAccounts serviceAccounts string? 'N/A]
[#:statusMessage statusMessage string? 'N/A]
[#:tags tags string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates an instance resource in the specified project using the data included in the request.

@racket[project]: Name of the project scoping this request.

@racket[id]: Unique identifier for the resource; defined by the server (output only).

@racket[kind]: Type of the resource.

@racket[image]: An optional URL of the disk image resource to be to be installed on this instance; provided by the client when the instance is created. If not specified, the server will choose a default image.

@racket[name]: Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.

@racket[description]: An optional textual description of the resource; provided by the client when the resource is created.

@racket[selfLink]: Server defined URL for the resource (output only).

@racket[status]: Instance status. One of the following values: "PROVISIONING", "STAGING", "RUNNING" (output only).

@racket[creationTimestamp]: Creation timestamp in RFC3339 text format (output only).

@racket[zone]: URL of the zone resource describing where this instance should be hosted; provided by the client when the instance is created.

@racket[disks]: Array of disks associated with this instance. Persistent disks must be created before you can assign them.

@racket[machineType]: URL of the machine type resource describing which machine type to use to host the instance; provided by the client when the instance is created.

@racket[metadata]: Metadata key/value pairs assigned to this instance. Consists of custom metadata or predefined keys; see Instance documentation for more information.

@racket[networkInterfaces]: Array of configurations for this interface. This specifies how this interface is configured to interact with other network services, such as connecting to the internet. Currently, ONE_TO_ONE_NAT is the only access config supported. If there are no accessConfigs specified, then this instance will have no external internet access.

@racket[serviceAccounts]: A list of service accounts each with specified scopes, for which access tokens are to be made available to the instance through metadata queries.

@racket[statusMessage]: An optional, human-readable explanation of the status (output only).

@racket[tags]: An optional set of tags applied to this instance. Used to identify valid sources or targets for network firewalls. Provided by the client when the instance is created. Each tag must be 1-63 characters long, and comply with RFC1035.

}

@defproc[(compute-instances-addAccessConfig
[#:project project string?]
[#:instance instance string?]
[#:network_interface network_interface string?]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:type type string? 'N/A]
[#:natIP natIP string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Adds an access config to an instance's network interface.

@racket[project]: Project name.

@racket[instance]: Instance name.

@racket[network_interface]: Network interface name.

@racket[kind]: Type of the resource.

@racket[name]: Name of this access configuration.

@racket[type]: Type of configuration. Must be set to "ONE_TO_ONE_NAT". This configures port-for-port NAT to the internet.

@racket[natIP]: An external IP address associated with this instance. Specify an unused static IP address available to the project. If left blank, the external IP will be drawn from a shared ephemeral pool.

}

@defproc[(compute-instances-deleteAccessConfig
[#:project project string?]
[#:instance instance string?]
[#:network_interface network_interface string?]
[#:access_config access_config string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes an access config from an instance's network interface.

@racket[project]: Project name.

@racket[instance]: Instance name.

@racket[network_interface]: Network interface name.

@racket[access_config]: Access config name.

}

@defproc[(compute-instances-delete
[#:project project string?]
[#:instance instance string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the specified instance resource.

@racket[project]: Name of the project scoping this request.

@racket[instance]: Name of the instance resource to delete.

}

@subsection{disks}
@defproc[(compute-disks-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of persistent disk resources contained within the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-disks-get
[#:disk disk string?]
[#:project project string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified persistent disk resource.

@racket[disk]: Name of the persistent disk resource to return.

@racket[project]: Name of the project scoping this request.

}

@defproc[(compute-disks-insert
[#:project project string?]
[#:options options string? 'N/A]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:status status string? 'N/A]
[#:creationTimestamp creationTimestamp string? 'N/A]
[#:sizeGb sizeGb string? 'N/A]
[#:sourceSnapshot sourceSnapshot string? 'N/A]
[#:sourceSnapshotId sourceSnapshotId string? 'N/A]
[#:zone zone string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a persistent disk resource in the specified project using the data included in the request.

@racket[project]: Name of the project scoping this request.

@racket[options]: Internal use only.

@racket[id]: Unique identifier for the resource; defined by the server (output only).

@racket[kind]: Type of the resource.

@racket[name]: Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.

@racket[description]: An optional textual description of the resource; provided by the client when the resource is created.

@racket[selfLink]: Server defined URL for the resource (output only).

@racket[status]: The status of disk creation (output only).

@racket[creationTimestamp]: Creation timestamp in RFC3339 text format (output only).

@racket[sizeGb]: Size of the persistent disk, specified in GB.

@racket[sourceSnapshot]: The source snapshot used to create this disk. Once the source snapshot has been deleted from the system, this field will be cleared, and will not be set even if a snapshot with the same name has been re-created.

@racket[sourceSnapshotId]: The 'id' value of the snapshot used to create this disk. This value may be used to determine whether the disk was created from the current or a previous instance of a given disk snapshot.

@racket[zone]: URL for the zone where the persistent disk resides; provided by the client when the disk is created. A persistent disk must reside in the same zone as the instance to which it is attached.

}

@defproc[(compute-disks-delete
[#:disk disk string?]
[#:project project string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the specified persistent disk resource.

@racket[disk]: Name of the persistent disk resource to delete.

@racket[project]: Name of the project scoping this request.

}

@subsection{firewalls}
@defproc[(compute-firewalls-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of firewall resources available to the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-firewalls-get
[#:project project string?]
[#:firewall firewall string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified firewall resource.

@racket[project]: Name of the project scoping this request.

@racket[firewall]: Name of the firewall resource to return.

}

@defproc[(compute-firewalls-insert
[#:project project string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:creationTimestamp creationTimestamp string? 'N/A]
[#:allowed allowed string? 'N/A]
[#:network network string? 'N/A]
[#:sourceRanges sourceRanges string? 'N/A]
[#:sourceTags sourceTags string? 'N/A]
[#:targetTags targetTags string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a firewall resource in the specified project using the data included in the request.

@racket[project]: Name of the project scoping this request.

@racket[id]: Unique identifier for the resource; defined by the server (output only).

@racket[kind]: Type of the resource.

@racket[name]: Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.

@racket[description]: An optional textual description of the resource; provided by the client when the resource is created.

@racket[selfLink]: Server defined URL for the resource (output only).

@racket[creationTimestamp]: Creation timestamp in RFC3339 text format (output only).

@racket[allowed]: The list of rules specified by this firewall. Each rule specifies a protocol and port-range tuple that describes a permitted connection.

@racket[network]: URL of the network to which this firewall is applied; provided by the client when the firewall is created.

@racket[sourceRanges]: A list of IP address blocks expressed in CIDR format which this rule applies to. One or both of sourceRanges and sourceTags may be set; an inbound connection is allowed if either the range or the tag of the source matches.

@racket[sourceTags]: A list of instance tags which this rule applies to. One or both of sourceRanges and sourceTags may be set; an inbound connection is allowed if either the range or the tag of the source matches.

@racket[targetTags]: A list of instance tags indicating sets of instances located on network which may make network connections as specified in allowed. If no targetTags are specified, the firewall rule applies to all instances on the specified network.

}

@defproc[(compute-firewalls-patch
[#:project project string?]
[#:firewall firewall string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:creationTimestamp creationTimestamp string? 'N/A]
[#:allowed allowed string? 'N/A]
[#:network network string? 'N/A]
[#:sourceRanges sourceRanges string? 'N/A]
[#:sourceTags sourceTags string? 'N/A]
[#:targetTags targetTags string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the specified firewall resource with the data included in the request. This method supports patch semantics.

@racket[project]: Name of the project scoping this request.

@racket[firewall]: Name of the firewall resource to update.

@racket[id]: Unique identifier for the resource; defined by the server (output only).

@racket[kind]: Type of the resource.

@racket[name]: Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.

@racket[description]: An optional textual description of the resource; provided by the client when the resource is created.

@racket[selfLink]: Server defined URL for the resource (output only).

@racket[creationTimestamp]: Creation timestamp in RFC3339 text format (output only).

@racket[allowed]: The list of rules specified by this firewall. Each rule specifies a protocol and port-range tuple that describes a permitted connection.

@racket[network]: URL of the network to which this firewall is applied; provided by the client when the firewall is created.

@racket[sourceRanges]: A list of IP address blocks expressed in CIDR format which this rule applies to. One or both of sourceRanges and sourceTags may be set; an inbound connection is allowed if either the range or the tag of the source matches.

@racket[sourceTags]: A list of instance tags which this rule applies to. One or both of sourceRanges and sourceTags may be set; an inbound connection is allowed if either the range or the tag of the source matches.

@racket[targetTags]: A list of instance tags indicating sets of instances located on network which may make network connections as specified in allowed. If no targetTags are specified, the firewall rule applies to all instances on the specified network.

}

@defproc[(compute-firewalls-update
[#:project project string?]
[#:firewall firewall string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:creationTimestamp creationTimestamp string? 'N/A]
[#:allowed allowed string? 'N/A]
[#:network network string? 'N/A]
[#:sourceRanges sourceRanges string? 'N/A]
[#:sourceTags sourceTags string? 'N/A]
[#:targetTags targetTags string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Updates the specified firewall resource with the data included in the request.

@racket[project]: Name of the project scoping this request.

@racket[firewall]: Name of the firewall resource to update.

@racket[id]: Unique identifier for the resource; defined by the server (output only).

@racket[kind]: Type of the resource.

@racket[name]: Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.

@racket[description]: An optional textual description of the resource; provided by the client when the resource is created.

@racket[selfLink]: Server defined URL for the resource (output only).

@racket[creationTimestamp]: Creation timestamp in RFC3339 text format (output only).

@racket[allowed]: The list of rules specified by this firewall. Each rule specifies a protocol and port-range tuple that describes a permitted connection.

@racket[network]: URL of the network to which this firewall is applied; provided by the client when the firewall is created.

@racket[sourceRanges]: A list of IP address blocks expressed in CIDR format which this rule applies to. One or both of sourceRanges and sourceTags may be set; an inbound connection is allowed if either the range or the tag of the source matches.

@racket[sourceTags]: A list of instance tags which this rule applies to. One or both of sourceRanges and sourceTags may be set; an inbound connection is allowed if either the range or the tag of the source matches.

@racket[targetTags]: A list of instance tags indicating sets of instances located on network which may make network connections as specified in allowed. If no targetTags are specified, the firewall rule applies to all instances on the specified network.

}

@defproc[(compute-firewalls-delete
[#:project project string?]
[#:firewall firewall string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the specified firewall resource.

@racket[project]: Name of the project scoping this request.

@racket[firewall]: Name of the firewall resource to delete.

}

@subsection{images}
@defproc[(compute-images-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of image resources available to the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-images-get
[#:image image string?]
[#:project project string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified image resource.

@racket[image]: Name of the image resource to return.

@racket[project]: Name of the project scoping this request.

}

@defproc[(compute-images-insert
[#:project project string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:creationTimestamp creationTimestamp string? 'N/A]
[#:diskSnapshot diskSnapshot string? 'N/A]
[#:preferredKernel preferredKernel string? 'N/A]
[#:rawDisk rawDisk string? 'N/A]
[#:sourceType sourceType string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates an image resource in the specified project using the data included in the request.

@racket[project]: Name of the project scoping this request.

@racket[id]: Unique identifier for the resource; defined by the server (output only).

@racket[kind]: Type of the resource.

@racket[name]: Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.

@racket[description]: Textual description of the resource; provided by the client when the resource is created.

@racket[selfLink]: Server defined URL for the resource (output only).

@racket[creationTimestamp]: Creation timestamp in RFC3339 text format (output only).

@racket[diskSnapshot]: Not yet implemented.

@racket[preferredKernel]: An optional URL of the preferred kernel for use with this disk image. If not specified, a server defined default kernel will be used.

@racket[rawDisk]: The raw disk image parameters.

@racket[sourceType]: Must be "RAW"; provided by the client when the disk image is created.

}

@defproc[(compute-images-delete
[#:image image string?]
[#:project project string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the specified image resource.

@racket[image]: Name of the image resource to delete.

@racket[project]: Name of the project scoping this request.

}

@subsection{kernels}
@defproc[(compute-kernels-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of kernel resources available to the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-kernels-get
[#:project project string?]
[#:kernel kernel string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified kernel resource.

@racket[project]: Name of the project scoping this request.

@racket[kernel]: Name of the kernel resource to return.

}

@subsection{machineTypes}
@defproc[(compute-machineTypes-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of machine type resources available to the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-machineTypes-get
[#:machineType machineType string?]
[#:project project string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified machine type resource.

@racket[machineType]: Name of the machine type resource to return.

@racket[project]: Name of the project scoping this request.

}

@subsection{networks}
@defproc[(compute-networks-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of network resources available to the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-networks-get
[#:network network string?]
[#:project project string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified network resource.

@racket[network]: Name of the network resource to return.

@racket[project]: Name of the project scoping this request.

}

@defproc[(compute-networks-insert
[#:project project string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:creationTimestamp creationTimestamp string? 'N/A]
[#:IPv4Range IPv4Range string? 'N/A]
[#:gatewayIPv4 gatewayIPv4 string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a network resource in the specified project using the data included in the request.

@racket[project]: Name of the project scoping this request.

@racket[id]: Unique identifier for the resource; defined by the server (output only).

@racket[kind]: Type of the resource.

@racket[name]: Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.

@racket[description]: An optional textual description of the resource; provided by the client when the resource is created.

@racket[selfLink]: Server defined URL for the resource (output only).

@racket[creationTimestamp]: Creation timestamp in RFC3339 text format (output only).

@racket[IPv4Range]: Required; The range of internal addresses that are legal on this network. This range is a CIDR specification, for example: 192.168.0.0/16. Provided by the client when the network is created.

@racket[gatewayIPv4]: An optional address that is used for default routing to other networks. This must be within the range specified by IPv4Range, and is typically the first usable address in that range. If not specified, the default value is the first usable address in IPv4Range.

}

@defproc[(compute-networks-delete
[#:network network string?]
[#:project project string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the specified network resource.

@racket[network]: Name of the network resource to delete.

@racket[project]: Name of the project scoping this request.

}

@subsection{operations}
@defproc[(compute-operations-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of operation resources contained within the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-operations-get
[#:project project string?]
[#:operation operation string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Retrieves the specified operation resource.

@racket[project]: Name of the project scoping this request.

@racket[operation]: Name of the operation resource to return.

}

@defproc[(compute-operations-delete
[#:project project string?]
[#:operation operation string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the specified operation resource.

@racket[project]: Name of the project scoping this request.

@racket[operation]: Name of the operation resource to delete.

}

@subsection{projects}
@defproc[(compute-projects-get
[#:project project string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified project resource.

@racket[project]: Name of the project resource to retrieve.

}

@defproc[(compute-projects-setCommonInstanceMetadata
[#:project project string?]
[#:kind kind string? 'N/A]
[#:items items string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Sets metadata common to all instances within the specified project using the data included in the request.

@racket[project]: Name of the project scoping this request.

@racket[kind]: Type of the resource.

@racket[items]: Array of key/value pairs. The total size of all keys and values must be less than 512 KB.

}

@subsection{snapshots}
@defproc[(compute-snapshots-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of persistent disk snapshot resources contained within the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-snapshots-get
[#:project project string?]
[#:snapshot snapshot string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified persistent disk snapshot resource.

@racket[project]: Name of the project scoping this request.

@racket[snapshot]: Name of the persistent disk snapshot resource to return.

}

@defproc[(compute-snapshots-insert
[#:project project string?]
[#:id id string? 'N/A]
[#:kind kind string? 'N/A]
[#:name name string? 'N/A]
[#:description description string? 'N/A]
[#:selfLink selfLink string? 'N/A]
[#:status status string? 'N/A]
[#:creationTimestamp creationTimestamp string? 'N/A]
[#:diskSizeGb diskSizeGb string? 'N/A]
[#:sourceDisk sourceDisk string? 'N/A]
[#:sourceDiskId sourceDiskId string? 'N/A]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Creates a persistent disk snapshot resource in the specified project using the data included in the request.

@racket[project]: Name of the project scoping this request.

@racket[id]: Unique identifier for the resource; defined by the server (output only).

@racket[kind]: Type of the resource.

@racket[name]: Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.

@racket[description]: An optional textual description of the resource; provided by the client when the resource is created.

@racket[selfLink]: Server defined URL for the resource (output only).

@racket[status]: The status of the persistent disk snapshot (output only).

@racket[creationTimestamp]: Creation timestamp in RFC3339 text format (output only).

@racket[diskSizeGb]: Size of the persistent disk snapshot, specified in GB (output only).

@racket[sourceDisk]: The source disk used to create this snapshot. Once the source disk has been deleted from the system, this field will be cleared, and will not be set even if a disk with the same name has been re-created.

@racket[sourceDiskId]: The 'id' value of the disk used to create this snapshot. This value may be used to determine whether the snapshot was taken from the current or a previous instance of a given disk name.

}

@defproc[(compute-snapshots-delete
[#:project project string?]
[#:snapshot snapshot string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Deletes the specified persistent disk snapshot resource.

@racket[project]: Name of the project scoping this request.

@racket[snapshot]: Name of the persistent disk snapshot resource to delete.

}

@subsection{zones}
@defproc[(compute-zones-list
[#:project project string?]
[#:filter filter string? 'N/A]
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
Retrieves the list of zone resources available to the specified project.

@racket[project]: Name of the project scoping this request.

@racket[filter]: Optional. Filter expression for filtering listed resources.

@racket[maxResults]: Optional. Maximum count of results to be returned. Maximum and default value is 100.

@racket[pageToken]: Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.

}

@defproc[(compute-zones-get
[#:zone zone string?]
[#:project project string?]
[#:fields fields string? 'N/A]
[#:key key string? (api-key)]
[#:alt alt string? 'N/A]
[#:oauth_token oauth_token string? 'N/A]
[#:prettyPrint prettyPrint string? 'N/A]
[#:quotaUser quotaUser string? 'N/A]
[#:userIp userIp string? 'N/A]
) jsexpr?]{
Returns the specified zone resource.

@racket[zone]: Name of the zone resource to return.

@racket[project]: Name of the project scoping this request.

}

