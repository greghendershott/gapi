{
 "kind": "discovery#restDescription",
 "discoveryVersion": "v1",
 "id": "compute:v1beta12",
 "name": "compute",
 "version": "v1beta12",
 "revision": "20120712",
 "title": "Compute Engine API",
 "description": "API for the Google Compute Engine service.",
 "icons": {
  "x16": "http://www.google.com/images/icons/product/compute_engine-16.png",
  "x32": "http://www.google.com/images/icons/product/compute_engine-32.png"
 },
 "documentationLink": "https://developers.google.com/compute/docs/reference/v1beta12",
 "labels": [
  "limited_availability"
 ],
 "protocol": "rest",
 "baseUrl": "https://www.googleapis.com/compute/v1beta12/projects/",
 "basePath": "/compute/v1beta12/projects/",
 "rootUrl": "https://www.googleapis.com/",
 "servicePath": "compute/v1beta12/projects/",
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
    "https://www.googleapis.com/auth/compute": {
     "description": "View and manage your Google Compute Engine resources"
    },
    "https://www.googleapis.com/auth/compute.readonly": {
     "description": "View your Google Compute Engine resources"
    },
    "https://www.googleapis.com/auth/devstorage.read_only": {
     "description": "View your data in Google Cloud Storage"
    }
   }
  }
 },
 "schemas": {
  "AccessConfig": {
   "id": "AccessConfig",
   "type": "object",
   "properties": {
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#accessConfig"
    },
    "name": {
     "type": "string",
     "description": "Name of this access configuration."
    },
    "natIP": {
     "type": "string",
     "description": "An external IP address associated with this instance. Specify an unused static IP address available to the project. If left blank, the external IP will be drawn from a shared ephemeral pool."
    },
    "type": {
     "type": "string",
     "description": "Type of configuration. Must be set to \"ONE_TO_ONE_NAT\". This configures port-for-port NAT to the internet.",
     "default": "ONE_TO_ONE_NAT"
    }
   }
  },
  "AttachedDisk": {
   "id": "AttachedDisk",
   "type": "object",
   "properties": {
    "deleteOnTerminate": {
     "type": "boolean",
     "description": "Persistent disk only; If true, delete the disk and all its data when the associated instance is deleted. This property defaults to false if not specified."
    },
    "deviceName": {
     "type": "string",
     "description": "Persistent disk only; must be unique within the instance when specified. This represents a unique device name that is reflected into the /dev/ tree of a Linux operating system running within the instance. If not specified, a default will be chosen by the system."
    },
    "index": {
     "type": "integer",
     "description": "A zero-based index to assign to this disk, where 0 is reserved for the boot disk. If not specified, the server will choose an appropriate value.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#attachedDisk"
    },
    "mode": {
     "type": "string",
     "description": "The mode in which to attach this disk, either \"READ_WRITE\" or \"READ_ONLY\"."
    },
    "source": {
     "type": "string",
     "description": "Persistent disk only; the URL of the persistent disk resource."
    },
    "type": {
     "type": "string",
     "description": "Type of the disk, either \"EPHEMERAL\" or \"PERSISTENT\". Note that persistent disks must be created before you can specify them here.",
     "annotations": {
      "required": [
       "compute.instances.insert"
      ]
     }
    }
   }
  },
  "Disk": {
   "id": "Disk",
   "type": "object",
   "properties": {
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "An optional textual description of the resource; provided by the client when the resource is created."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#disk"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.",
     "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
     "annotations": {
      "required": [
       "compute.disks.insert"
      ]
     }
    },
    "options": {
     "type": "string",
     "description": "Internal use only."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    },
    "sizeGb": {
     "type": "string",
     "description": "Size of the persistent disk, specified in GB.",
     "format": "int64",
     "annotations": {
      "required": [
       "compute.disks.insert"
      ]
     }
    },
    "sourceSnapshot": {
     "type": "string",
     "description": "The source snapshot used to create this disk. Once the source snapshot has been deleted from the system, this field will be cleared, and will not be set even if a snapshot with the same name has been re-created."
    },
    "sourceSnapshotId": {
     "type": "string",
     "description": "The 'id' value of the snapshot used to create this disk. This value may be used to determine whether the disk was created from the current or a previous instance of a given disk snapshot."
    },
    "status": {
     "type": "string",
     "description": "The status of disk creation (output only)."
    },
    "zone": {
     "type": "string",
     "description": "URL for the zone where the persistent disk resides; provided by the client when the disk is created. A persistent disk must reside in the same zone as the instance to which it is attached.",
     "annotations": {
      "required": [
       "compute.disks.insert"
      ]
     }
    }
   }
  },
  "DiskList": {
   "id": "DiskList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "The persistent disk resources.",
     "items": {
      "$ref": "Disk"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#diskList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  },
  "Firewall": {
   "id": "Firewall",
   "type": "object",
   "properties": {
    "allowed": {
     "type": "array",
     "description": "The list of rules specified by this firewall. Each rule specifies a protocol and port-range tuple that describes a permitted connection.",
     "items": {
      "type": "object",
      "properties": {
       "IPProtocol": {
        "type": "string",
        "description": "Required; this is the IP protocol that is allowed for this rule. This can either be a well known protocol string (tcp, udp or icmp) or the IP protocol number."
       },
       "ports": {
        "type": "array",
        "description": "An optional list of ports which are allowed. It is an error to specify this for any protocol that isn't UDP or TCP. Each entry must be either an integer or a range. If not specified, connections through any port are allowed.\nExample inputs include: [\"22\"], [\"80,\"443\"] and [\"12345-12349\"].",
        "items": {
         "type": "string"
        }
       }
      }
     }
    },
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "An optional textual description of the resource; provided by the client when the resource is created."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#firewall"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.",
     "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
     "annotations": {
      "required": [
       "compute.firewalls.insert",
       "compute.firewalls.patch"
      ]
     }
    },
    "network": {
     "type": "string",
     "description": "URL of the network to which this firewall is applied; provided by the client when the firewall is created.",
     "annotations": {
      "required": [
       "compute.firewalls.insert",
       "compute.firewalls.patch"
      ]
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    },
    "sourceRanges": {
     "type": "array",
     "description": "A list of IP address blocks expressed in CIDR format which this rule applies to. One or both of sourceRanges and sourceTags may be set; an inbound connection is allowed if either the range or the tag of the source matches.",
     "items": {
      "type": "string",
      "pattern": "[0-9]{1,3}(?:\\.[0-9]{1,3}){3}/[0-9]{1,2}"
     }
    },
    "sourceTags": {
     "type": "array",
     "description": "A list of instance tags which this rule applies to. One or both of sourceRanges and sourceTags may be set; an inbound connection is allowed if either the range or the tag of the source matches.",
     "items": {
      "type": "string",
      "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?"
     }
    },
    "targetTags": {
     "type": "array",
     "description": "A list of instance tags indicating sets of instances located on network which may make network connections as specified in allowed. If no targetTags are specified, the firewall rule applies to all instances on the specified network.",
     "items": {
      "type": "string",
      "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?"
     }
    }
   }
  },
  "FirewallList": {
   "id": "FirewallList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "The firewall resources.",
     "items": {
      "$ref": "Firewall"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#firewallList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  },
  "Image": {
   "id": "Image",
   "type": "object",
   "properties": {
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "Textual description of the resource; provided by the client when the resource is created."
    },
    "diskSnapshot": {
     "type": "object",
     "description": "Not yet implemented.",
     "properties": {
      "source": {
       "type": "string",
       "description": "URL of the disk snapshot.",
       "annotations": {
        "required": [
         "compute.images.insert"
        ]
       }
      }
     }
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#image"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.",
     "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
     "annotations": {
      "required": [
       "compute.images.insert"
      ]
     }
    },
    "preferredKernel": {
     "type": "string",
     "description": "An optional URL of the preferred kernel for use with this disk image. If not specified, a server defined default kernel will be used."
    },
    "rawDisk": {
     "type": "object",
     "description": "The raw disk image parameters.",
     "properties": {
      "containerType": {
       "type": "string",
       "description": "The format used to encode and transmit the block device. Should be TAR. This is just a container and transmission format and not a runtime format. Provided by the client when the disk image is created.",
       "default": "TAR",
       "annotations": {
        "required": [
         "compute.images.insert"
        ]
       }
      },
      "sha1Checksum": {
       "type": "string",
       "description": "An optional SHA1 checksum of the disk image before unpackaging; provided by the client when the disk image is created.",
       "pattern": "[a-f0-9]{40}"
      },
      "source": {
       "type": "string",
       "description": "The full Google Cloud Storage URL where the disk image is stored; provided by the client when the disk image is created.",
       "annotations": {
        "required": [
         "compute.images.insert"
        ]
       }
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    },
    "sourceType": {
     "type": "string",
     "description": "Must be \"RAW\"; provided by the client when the disk image is created.",
     "default": "RAW",
     "annotations": {
      "required": [
       "compute.images.insert"
      ]
     }
    }
   }
  },
  "ImageList": {
   "id": "ImageList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "The disk image resources.",
     "items": {
      "$ref": "Image"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#imageList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  },
  "Instance": {
   "id": "Instance",
   "type": "object",
   "properties": {
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "An optional textual description of the resource; provided by the client when the resource is created."
    },
    "disks": {
     "type": "array",
     "description": "Array of disks associated with this instance. Persistent disks must be created before you can assign them.",
     "items": {
      "$ref": "AttachedDisk"
     }
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "image": {
     "type": "string",
     "description": "An optional URL of the disk image resource to be to be installed on this instance; provided by the client when the instance is created. If not specified, the server will choose a default image.",
     "annotations": {
      "required": [
       "compute.instances.insert"
      ]
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#instance"
    },
    "machineType": {
     "type": "string",
     "description": "URL of the machine type resource describing which machine type to use to host the instance; provided by the client when the instance is created.",
     "annotations": {
      "required": [
       "compute.instances.insert"
      ]
     }
    },
    "metadata": {
     "$ref": "Metadata",
     "description": "Metadata key/value pairs assigned to this instance. Consists of custom metadata or predefined keys; see Instance documentation for more information."
    },
    "name": {
     "type": "string",
     "description": "Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.",
     "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
     "annotations": {
      "required": [
       "compute.instances.insert"
      ]
     }
    },
    "networkInterfaces": {
     "type": "array",
     "description": "Array of configurations for this interface. This specifies how this interface is configured to interact with other network services, such as connecting to the internet. Currently, ONE_TO_ONE_NAT is the only access config supported. If there are no accessConfigs specified, then this instance will have no external internet access.",
     "items": {
      "$ref": "NetworkInterface"
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    },
    "serviceAccounts": {
     "type": "array",
     "description": "A list of service accounts each with specified scopes, for which access tokens are to be made available to the instance through metadata queries.",
     "items": {
      "$ref": "ServiceAccount"
     }
    },
    "status": {
     "type": "string",
     "description": "Instance status. One of the following values: \"PROVISIONING\", \"STAGING\", \"RUNNING\" (output only)."
    },
    "statusMessage": {
     "type": "string",
     "description": "An optional, human-readable explanation of the status (output only)."
    },
    "tags": {
     "type": "array",
     "description": "An optional set of tags applied to this instance. Used to identify valid sources or targets for network firewalls. Provided by the client when the instance is created. Each tag must be 1-63 characters long, and comply with RFC1035.",
     "items": {
      "type": "string",
      "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?"
     }
    },
    "zone": {
     "type": "string",
     "description": "URL of the zone resource describing where this instance should be hosted; provided by the client when the instance is created.",
     "annotations": {
      "required": [
       "compute.instances.insert"
      ]
     }
    }
   }
  },
  "InstanceList": {
   "id": "InstanceList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "A list of instance resources.",
     "items": {
      "$ref": "Instance"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#instanceList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  },
  "Kernel": {
   "id": "Kernel",
   "type": "object",
   "properties": {
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "An optional textual description of the resource."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#kernel"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource.",
     "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?"
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    }
   }
  },
  "KernelList": {
   "id": "KernelList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "The kernel resources.",
     "items": {
      "$ref": "Kernel"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#kernelList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  },
  "MachineType": {
   "id": "MachineType",
   "type": "object",
   "properties": {
    "availableZone": {
     "type": "array",
     "description": "The zones that this machine type can run in.",
     "items": {
      "type": "any"
     }
    },
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "An optional textual description of the resource."
    },
    "ephemeralDisks": {
     "type": "array",
     "description": "List of extended ephemeral disks assigned to the instance.",
     "items": {
      "type": "object",
      "properties": {
       "diskGb": {
        "type": "integer",
        "description": "Size of the ephemeral disk, defined in GB.",
        "format": "int32"
       }
      }
     }
    },
    "guestCpus": {
     "type": "integer",
     "description": "Count of CPUs exposed to the instance.",
     "format": "int32"
    },
    "hostCpus": {
     "type": "integer",
     "description": "Count of physical CPUs reserved on the virtual machine host. Deprecated.",
     "format": "int32"
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "imageSpaceGb": {
     "type": "integer",
     "description": "Space allotted for the image, defined in GB.",
     "format": "int32"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#machineType"
    },
    "maximumPersistentDisks": {
     "type": "integer",
     "description": "Maximum persistent disks allowed.",
     "format": "int32"
    },
    "maximumPersistentDisksSizeGb": {
     "type": "string",
     "description": "Maximum total persistent disks size (GB) allowed.",
     "format": "int64"
    },
    "memoryMb": {
     "type": "integer",
     "description": "Physical memory assigned to the instance, defined in MB.",
     "format": "int32"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource.",
     "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?"
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    }
   }
  },
  "MachineTypeList": {
   "id": "MachineTypeList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "The machine type resources.",
     "items": {
      "$ref": "MachineType"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#machineTypeList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  },
  "Metadata": {
   "id": "Metadata",
   "type": "object",
   "properties": {
    "items": {
     "type": "array",
     "description": "Array of key/value pairs. The total size of all keys and values must be less than 512 KB.",
     "items": {
      "type": "object",
      "properties": {
       "key": {
        "type": "string",
        "description": "Key for the metadata entry. Keys must conform to the following regexp: [a-zA-Z0-9-_]+, and be less than 128 bytes in length. This is reflected as part of a URL in the metadata server. Additionally, to avoid ambiguity, keys must be unique.",
        "pattern": "[a-zA-Z0-9-_]{1,128}",
        "annotations": {
         "required": [
          "compute.instances.insert",
          "compute.projects.setCommonInstanceMetadata"
         ]
        }
       },
       "value": {
        "type": "string",
        "description": "Value for the metadata entry. These are free-form strings, and only have meaning as interpreted by the image running in the instance. The only restriction placed on values is that their size must be less than or equal to 15000 bytes.",
        "annotations": {
         "required": [
          "compute.instances.insert",
          "compute.projects.setCommonInstanceMetadata"
         ]
        }
       }
      }
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#metadata"
    }
   }
  },
  "Network": {
   "id": "Network",
   "type": "object",
   "properties": {
    "IPv4Range": {
     "type": "string",
     "description": "Required; The range of internal addresses that are legal on this network. This range is a CIDR specification, for example: 192.168.0.0/16. Provided by the client when the network is created.",
     "pattern": "[0-9]{1,3}(?:\\.[0-9]{1,3}){3}/[0-9]{1,2}",
     "annotations": {
      "required": [
       "compute.networks.insert"
      ]
     }
    },
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "An optional textual description of the resource; provided by the client when the resource is created."
    },
    "gatewayIPv4": {
     "type": "string",
     "description": "An optional address that is used for default routing to other networks. This must be within the range specified by IPv4Range, and is typically the first usable address in that range. If not specified, the default value is the first usable address in IPv4Range.",
     "pattern": "[0-9]{1,3}(?:\\.[0-9]{1,3}){3}"
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#network"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.",
     "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
     "annotations": {
      "required": [
       "compute.networks.insert"
      ]
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    }
   }
  },
  "NetworkInterface": {
   "id": "NetworkInterface",
   "type": "object",
   "properties": {
    "accessConfigs": {
     "type": "array",
     "description": "Array of configurations for this interface. This specifies how this interface is configured to interact with other network services, such as connecting to the internet. Currently, ONE_TO_ONE_NAT is the only access config supported. If there are no accessConfigs specified, then this instance will have no external internet access.",
     "items": {
      "$ref": "AccessConfig"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#networkInterface"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource, determined by the server; for network devices, these are e.g. eth0, eth1, etc. (output only)."
    },
    "network": {
     "type": "string",
     "description": "URL of the network resource attached to this interface.",
     "annotations": {
      "required": [
       "compute.instances.insert"
      ]
     }
    },
    "networkIP": {
     "type": "string",
     "description": "An optional IPV4 internal network address to assign to this instance. If not specified, one will be assigned from the available range."
    }
   }
  },
  "NetworkList": {
   "id": "NetworkList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "The network resources.",
     "items": {
      "$ref": "Network"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#networkList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  },
  "Operation": {
   "id": "Operation",
   "type": "object",
   "properties": {
    "clientOperationId": {
     "type": "string",
     "description": "An optional identifier specified by the client when the mutation was initiated. Must be unique for all operation resources in the project (output only)."
    },
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "endTime": {
     "type": "string",
     "description": "The time that this operation was completed. This is in RFC 3339 format (output only)."
    },
    "error": {
     "type": "object",
     "description": "If errors occurred during processing of this operation, this field will be populated (output only).",
     "properties": {
      "errors": {
       "type": "array",
       "description": "The array of errors encountered while processing this operation.",
       "items": {
        "type": "object",
        "properties": {
         "code": {
          "type": "string",
          "description": "The error type identifier for this error."
         },
         "location": {
          "type": "string",
          "description": "Indicates the field in the request which caused the error. This property is optional."
         },
         "message": {
          "type": "string",
          "description": "An optional, human-readable error message."
         }
        }
       }
      }
     }
    },
    "httpErrorMessage": {
     "type": "string",
     "description": "If operation fails, the HTTP error message returned, e.g. NOT FOUND. (output only)."
    },
    "httpErrorStatusCode": {
     "type": "integer",
     "description": "If operation fails, the HTTP error status code returned, e.g. 404. (output only).",
     "format": "int32"
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "insertTime": {
     "type": "string",
     "description": "The time that this operation was requested. This is in RFC 3339 format (output only)."
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#operation"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource."
    },
    "operationType": {
     "type": "string",
     "description": "Type of the operation. Examples include \"insert\", \"update\", and \"delete\" (output only)."
    },
    "progress": {
     "type": "integer",
     "description": "An optional progress indicator that ranges from 0 to 100. There is no requirement that this be linear or support any granularity of operations. This should not be used to guess at when the operation will be complete. This number should be monotonically increasing as the operation progresses (output only).",
     "format": "int32"
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    },
    "startTime": {
     "type": "string",
     "description": "The time that this operation was started by the server. This is in RFC 3339 format (output only)."
    },
    "status": {
     "type": "string",
     "description": "Status of the operation. Can be one of the following: \"PENDING\", \"RUNNING\", or \"DONE\" (output only)."
    },
    "statusMessage": {
     "type": "string",
     "description": "An optional textual description of the current status of the operation (output only)."
    },
    "targetId": {
     "type": "string",
     "description": "Unique target id which identifies a particular incarnation of the target (output only).",
     "format": "uint64"
    },
    "targetLink": {
     "type": "string",
     "description": "URL of the resource the operation is mutating (output only)."
    },
    "user": {
     "type": "string",
     "description": "User who requested the operation, for example \"user@example.com\" (output only)."
    }
   }
  },
  "OperationList": {
   "id": "OperationList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "The operation resources.",
     "items": {
      "$ref": "Operation"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#operationList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  },
  "Project": {
   "id": "Project",
   "type": "object",
   "properties": {
    "commonInstanceMetadata": {
     "$ref": "Metadata",
     "description": "Metadata key/value pairs available to all instances contained in this project."
    },
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "An optional textual description of the resource."
    },
    "externalIpAddresses": {
     "type": "array",
     "description": "Internet available IP addresses available for use in this project.",
     "items": {
      "type": "string"
     }
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#project"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource."
    },
    "quotas": {
     "type": "array",
     "description": "Quotas assigned to this project.",
     "items": {
      "type": "object",
      "properties": {
       "limit": {
        "type": "number",
        "description": "Quota limit for this metric.",
        "format": "double"
       },
       "metric": {
        "type": "string",
        "description": "Name of the quota metric."
       },
       "usage": {
        "type": "number",
        "description": "Current usage of this metric.",
        "format": "double"
       }
      }
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    }
   }
  },
  "ServiceAccount": {
   "id": "ServiceAccount",
   "type": "object",
   "properties": {
    "email": {
     "type": "string",
     "description": "Email address of the service account."
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#serviceAccount"
    },
    "scopes": {
     "type": "array",
     "description": "The list of scopes to be made available for this service account.",
     "items": {
      "type": "string"
     }
    }
   }
  },
  "Snapshot": {
   "id": "Snapshot",
   "type": "object",
   "properties": {
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "An optional textual description of the resource; provided by the client when the resource is created."
    },
    "diskSizeGb": {
     "type": "string",
     "description": "Size of the persistent disk snapshot, specified in GB (output only).",
     "format": "int64"
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#snapshot"
    },
    "name": {
     "type": "string",
     "description": "Name of the resource; provided by the client when the resource is created. The name must be 1-63 characters long, and comply with RFC1035.",
     "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
     "annotations": {
      "required": [
       "compute.snapshots.insert"
      ]
     }
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    },
    "sourceDisk": {
     "type": "string",
     "description": "The source disk used to create this snapshot. Once the source disk has been deleted from the system, this field will be cleared, and will not be set even if a disk with the same name has been re-created."
    },
    "sourceDiskId": {
     "type": "string",
     "description": "The 'id' value of the disk used to create this snapshot. This value may be used to determine whether the snapshot was taken from the current or a previous instance of a given disk name."
    },
    "status": {
     "type": "string",
     "description": "The status of the persistent disk snapshot (output only)."
    }
   }
  },
  "SnapshotList": {
   "id": "SnapshotList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "The persistent snapshot resources.",
     "items": {
      "$ref": "Snapshot"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#snapshotList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  },
  "Zone": {
   "id": "Zone",
   "type": "object",
   "properties": {
    "availableMachineType": {
     "type": "array",
     "description": "The machine types that can be used in this zone (output only).",
     "items": {
      "type": "any"
     }
    },
    "creationTimestamp": {
     "type": "string",
     "description": "Creation timestamp in RFC3339 text format (output only)."
    },
    "description": {
     "type": "string",
     "description": "Textual description of the resource."
    },
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only).",
     "format": "uint64"
    },
    "kind": {
     "type": "string",
     "description": "Type of the resource.",
     "default": "compute#zone"
    },
    "maintenanceWindows": {
     "type": "array",
     "description": "Scheduled maintenance windows for the zone. When the zone is in a maintenance window, all resources which reside in the zone will be unavailable.",
     "items": {
      "type": "object",
      "properties": {
       "beginTime": {
        "type": "string",
        "description": "Begin time of the maintenance window, in RFC 3339 format."
       },
       "description": {
        "type": "string",
        "description": "Textual description of the maintenance window."
       },
       "endTime": {
        "type": "string",
        "description": "End time of the maintenance window, in RFC 3339 format."
       },
       "name": {
        "type": "string",
        "description": "Name of the maintenance window."
       }
      }
     }
    },
    "name": {
     "type": "string",
     "description": "Name of the resource."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for the resource (output only)."
    },
    "status": {
     "type": "string",
     "description": "Status of the zone. \"UP\" or \"DOWN\"."
    }
   }
  },
  "ZoneList": {
   "id": "ZoneList",
   "type": "object",
   "properties": {
    "id": {
     "type": "string",
     "description": "Unique identifier for the resource; defined by the server (output only)."
    },
    "items": {
     "type": "array",
     "description": "The zone resources.",
     "items": {
      "$ref": "Zone"
     }
    },
    "kind": {
     "type": "string",
     "description": "Type of resource.",
     "default": "compute#zoneList"
    },
    "nextPageToken": {
     "type": "string",
     "description": "A token used to continue a truncated list request (output only)."
    },
    "selfLink": {
     "type": "string",
     "description": "Server defined URL for this resource (output only)."
    }
   }
  }
 },
 "resources": {
  "disks": {
   "methods": {
    "delete": {
     "id": "compute.disks.delete",
     "path": "{project}/disks/{disk}",
     "httpMethod": "DELETE",
     "description": "Deletes the specified persistent disk resource.",
     "parameters": {
      "disk": {
       "type": "string",
       "description": "Name of the persistent disk resource to delete.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "disk"
     ],
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "get": {
     "id": "compute.disks.get",
     "path": "{project}/disks/{disk}",
     "httpMethod": "GET",
     "description": "Returns the specified persistent disk resource.",
     "parameters": {
      "disk": {
       "type": "string",
       "description": "Name of the persistent disk resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "disk"
     ],
     "response": {
      "$ref": "Disk"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "insert": {
     "id": "compute.disks.insert",
     "path": "{project}/disks",
     "httpMethod": "POST",
     "description": "Creates a persistent disk resource in the specified project using the data included in the request.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "request": {
      "$ref": "Disk"
     },
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "list": {
     "id": "compute.disks.list",
     "path": "{project}/disks",
     "httpMethod": "GET",
     "description": "Retrieves the list of persistent disk resources contained within the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "DiskList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    }
   }
  },
  "firewalls": {
   "methods": {
    "delete": {
     "id": "compute.firewalls.delete",
     "path": "{project}/firewalls/{firewall}",
     "httpMethod": "DELETE",
     "description": "Deletes the specified firewall resource.",
     "parameters": {
      "firewall": {
       "type": "string",
       "description": "Name of the firewall resource to delete.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "firewall"
     ],
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "get": {
     "id": "compute.firewalls.get",
     "path": "{project}/firewalls/{firewall}",
     "httpMethod": "GET",
     "description": "Returns the specified firewall resource.",
     "parameters": {
      "firewall": {
       "type": "string",
       "description": "Name of the firewall resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "firewall"
     ],
     "response": {
      "$ref": "Firewall"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "insert": {
     "id": "compute.firewalls.insert",
     "path": "{project}/firewalls",
     "httpMethod": "POST",
     "description": "Creates a firewall resource in the specified project using the data included in the request.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "request": {
      "$ref": "Firewall"
     },
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "list": {
     "id": "compute.firewalls.list",
     "path": "{project}/firewalls",
     "httpMethod": "GET",
     "description": "Retrieves the list of firewall resources available to the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "FirewallList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "patch": {
     "id": "compute.firewalls.patch",
     "path": "{project}/firewalls/{firewall}",
     "httpMethod": "PATCH",
     "description": "Updates the specified firewall resource with the data included in the request. This method supports patch semantics.",
     "parameters": {
      "firewall": {
       "type": "string",
       "description": "Name of the firewall resource to update.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "firewall"
     ],
     "request": {
      "$ref": "Firewall"
     },
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "update": {
     "id": "compute.firewalls.update",
     "path": "{project}/firewalls/{firewall}",
     "httpMethod": "PUT",
     "description": "Updates the specified firewall resource with the data included in the request.",
     "parameters": {
      "firewall": {
       "type": "string",
       "description": "Name of the firewall resource to update.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "firewall"
     ],
     "request": {
      "$ref": "Firewall"
     },
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    }
   }
  },
  "images": {
   "methods": {
    "delete": {
     "id": "compute.images.delete",
     "path": "{project}/images/{image}",
     "httpMethod": "DELETE",
     "description": "Deletes the specified image resource.",
     "parameters": {
      "image": {
       "type": "string",
       "description": "Name of the image resource to delete.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "image"
     ],
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "get": {
     "id": "compute.images.get",
     "path": "{project}/images/{image}",
     "httpMethod": "GET",
     "description": "Returns the specified image resource.",
     "parameters": {
      "image": {
       "type": "string",
       "description": "Name of the image resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "image"
     ],
     "response": {
      "$ref": "Image"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "insert": {
     "id": "compute.images.insert",
     "path": "{project}/images",
     "httpMethod": "POST",
     "description": "Creates an image resource in the specified project using the data included in the request.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "request": {
      "$ref": "Image"
     },
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute",
      "https://www.googleapis.com/auth/devstorage.read_only"
     ]
    },
    "list": {
     "id": "compute.images.list",
     "path": "{project}/images",
     "httpMethod": "GET",
     "description": "Retrieves the list of image resources available to the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "ImageList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    }
   }
  },
  "instances": {
   "methods": {
    "addAccessConfig": {
     "id": "compute.instances.addAccessConfig",
     "path": "{project}/instances/{instance}/add-access-config",
     "httpMethod": "POST",
     "description": "Adds an access config to an instance's network interface.",
     "parameters": {
      "instance": {
       "type": "string",
       "description": "Instance name.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "network_interface": {
       "type": "string",
       "description": "Network interface name.",
       "required": true,
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Project name.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "instance",
      "network_interface"
     ],
     "request": {
      "$ref": "AccessConfig"
     },
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "delete": {
     "id": "compute.instances.delete",
     "path": "{project}/instances/{instance}",
     "httpMethod": "DELETE",
     "description": "Deletes the specified instance resource.",
     "parameters": {
      "instance": {
       "type": "string",
       "description": "Name of the instance resource to delete.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "instance"
     ],
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "deleteAccessConfig": {
     "id": "compute.instances.deleteAccessConfig",
     "path": "{project}/instances/{instance}/delete-access-config",
     "httpMethod": "POST",
     "description": "Deletes an access config from an instance's network interface.",
     "parameters": {
      "access_config": {
       "type": "string",
       "description": "Access config name.",
       "required": true,
       "location": "query"
      },
      "instance": {
       "type": "string",
       "description": "Instance name.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "network_interface": {
       "type": "string",
       "description": "Network interface name.",
       "required": true,
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Project name.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "instance",
      "access_config",
      "network_interface"
     ],
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "get": {
     "id": "compute.instances.get",
     "path": "{project}/instances/{instance}",
     "httpMethod": "GET",
     "description": "Returns the specified instance resource.",
     "parameters": {
      "instance": {
       "type": "string",
       "description": "Name of the instance resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "instance"
     ],
     "response": {
      "$ref": "Instance"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "insert": {
     "id": "compute.instances.insert",
     "path": "{project}/instances",
     "httpMethod": "POST",
     "description": "Creates an instance resource in the specified project using the data included in the request.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "request": {
      "$ref": "Instance"
     },
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "list": {
     "id": "compute.instances.list",
     "path": "{project}/instances",
     "httpMethod": "GET",
     "description": "Retrieves the list of instance resources contained within the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "InstanceList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    }
   }
  },
  "kernels": {
   "methods": {
    "get": {
     "id": "compute.kernels.get",
     "path": "{project}/kernels/{kernel}",
     "httpMethod": "GET",
     "description": "Returns the specified kernel resource.",
     "parameters": {
      "kernel": {
       "type": "string",
       "description": "Name of the kernel resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "kernel"
     ],
     "response": {
      "$ref": "Kernel"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "list": {
     "id": "compute.kernels.list",
     "path": "{project}/kernels",
     "httpMethod": "GET",
     "description": "Retrieves the list of kernel resources available to the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "KernelList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    }
   }
  },
  "machineTypes": {
   "methods": {
    "get": {
     "id": "compute.machineTypes.get",
     "path": "{project}/machine-types/{machineType}",
     "httpMethod": "GET",
     "description": "Returns the specified machine type resource.",
     "parameters": {
      "machineType": {
       "type": "string",
       "description": "Name of the machine type resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "machineType"
     ],
     "response": {
      "$ref": "MachineType"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "list": {
     "id": "compute.machineTypes.list",
     "path": "{project}/machine-types",
     "httpMethod": "GET",
     "description": "Retrieves the list of machine type resources available to the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "MachineTypeList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    }
   }
  },
  "networks": {
   "methods": {
    "delete": {
     "id": "compute.networks.delete",
     "path": "{project}/networks/{network}",
     "httpMethod": "DELETE",
     "description": "Deletes the specified network resource.",
     "parameters": {
      "network": {
       "type": "string",
       "description": "Name of the network resource to delete.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "network"
     ],
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "get": {
     "id": "compute.networks.get",
     "path": "{project}/networks/{network}",
     "httpMethod": "GET",
     "description": "Returns the specified network resource.",
     "parameters": {
      "network": {
       "type": "string",
       "description": "Name of the network resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "network"
     ],
     "response": {
      "$ref": "Network"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "insert": {
     "id": "compute.networks.insert",
     "path": "{project}/networks",
     "httpMethod": "POST",
     "description": "Creates a network resource in the specified project using the data included in the request.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "request": {
      "$ref": "Network"
     },
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "list": {
     "id": "compute.networks.list",
     "path": "{project}/networks",
     "httpMethod": "GET",
     "description": "Retrieves the list of network resources available to the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "NetworkList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    }
   }
  },
  "operations": {
   "methods": {
    "delete": {
     "id": "compute.operations.delete",
     "path": "{project}/operations/{operation}",
     "httpMethod": "DELETE",
     "description": "Deletes the specified operation resource.",
     "parameters": {
      "operation": {
       "type": "string",
       "description": "Name of the operation resource to delete.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "operation"
     ],
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "get": {
     "id": "compute.operations.get",
     "path": "{project}/operations/{operation}",
     "httpMethod": "GET",
     "description": "Retrieves the specified operation resource.",
     "parameters": {
      "operation": {
       "type": "string",
       "description": "Name of the operation resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "operation"
     ],
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "list": {
     "id": "compute.operations.list",
     "path": "{project}/operations",
     "httpMethod": "GET",
     "description": "Retrieves the list of operation resources contained within the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "OperationList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    }
   }
  },
  "projects": {
   "methods": {
    "get": {
     "id": "compute.projects.get",
     "path": "{project}",
     "httpMethod": "GET",
     "description": "Returns the specified project resource.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project resource to retrieve.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "Project"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "setCommonInstanceMetadata": {
     "id": "compute.projects.setCommonInstanceMetadata",
     "path": "{project}/set-common-instance-metadata",
     "httpMethod": "POST",
     "description": "Sets metadata common to all instances within the specified project using the data included in the request.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "request": {
      "$ref": "Metadata"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    }
   }
  },
  "snapshots": {
   "methods": {
    "delete": {
     "id": "compute.snapshots.delete",
     "path": "{project}/snapshots/{snapshot}",
     "httpMethod": "DELETE",
     "description": "Deletes the specified persistent disk snapshot resource.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      },
      "snapshot": {
       "type": "string",
       "description": "Name of the persistent disk snapshot resource to delete.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "snapshot"
     ],
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "get": {
     "id": "compute.snapshots.get",
     "path": "{project}/snapshots/{snapshot}",
     "httpMethod": "GET",
     "description": "Returns the specified persistent disk snapshot resource.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      },
      "snapshot": {
       "type": "string",
       "description": "Name of the persistent disk snapshot resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "snapshot"
     ],
     "response": {
      "$ref": "Snapshot"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "insert": {
     "id": "compute.snapshots.insert",
     "path": "{project}/snapshots",
     "httpMethod": "POST",
     "description": "Creates a persistent disk snapshot resource in the specified project using the data included in the request.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "request": {
      "$ref": "Snapshot"
     },
     "response": {
      "$ref": "Operation"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute"
     ]
    },
    "list": {
     "id": "compute.snapshots.list",
     "path": "{project}/snapshots",
     "httpMethod": "GET",
     "description": "Retrieves the list of persistent disk snapshot resources contained within the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "SnapshotList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    }
   }
  },
  "zones": {
   "methods": {
    "get": {
     "id": "compute.zones.get",
     "path": "{project}/zones/{zone}",
     "httpMethod": "GET",
     "description": "Returns the specified zone resource.",
     "parameters": {
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      },
      "zone": {
       "type": "string",
       "description": "Name of the zone resource to return.",
       "required": true,
       "pattern": "[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project",
      "zone"
     ],
     "response": {
      "$ref": "Zone"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    },
    "list": {
     "id": "compute.zones.list",
     "path": "{project}/zones",
     "httpMethod": "GET",
     "description": "Retrieves the list of zone resources available to the specified project.",
     "parameters": {
      "filter": {
       "type": "string",
       "description": "Optional. Filter expression for filtering listed resources.",
       "location": "query"
      },
      "maxResults": {
       "type": "integer",
       "description": "Optional. Maximum count of results to be returned. Maximum and default value is 100.",
       "default": "100",
       "format": "uint32",
       "minimum": "0",
       "maximum": "100",
       "location": "query"
      },
      "pageToken": {
       "type": "string",
       "description": "Optional. Tag returned by a previous list request truncated by maxResults. Used to continue a previous list request.",
       "location": "query"
      },
      "project": {
       "type": "string",
       "description": "Name of the project scoping this request.",
       "required": true,
       "pattern": "(?:(?:[-a-z0-9]{1,63}\\.)*(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?):)?(?:[0-9]{1,19}|(?:[a-z](?:[-a-z0-9]{0,61}[a-z0-9])?))",
       "location": "path"
      }
     },
     "parameterOrder": [
      "project"
     ],
     "response": {
      "$ref": "ZoneList"
     },
     "scopes": [
      "https://www.googleapis.com/auth/compute.readonly"
     ]
    }
   }
  }
 }
}
