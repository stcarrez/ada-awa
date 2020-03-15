# Workspaces Module
The *workspaces* plugin defines a workspace area for other plugins.
The workspace is intended to link together all the data objects that an application
manages for a given user or group of users.  A workspace is a possible mechanism
to provide and implement multi-tenancy in a web application.  By using the workspace plugin,
application data from different customers can be kept separate from each other in the
same database.

## Events
The *workspaces* module provides several events that are posted when some action are performed.

### invite-user
This event is posted when an invitation is created for a user.  The event can be used to
send the associated invitation email to the invitee.  The event contains the following
attributes:

key
email
name
message
inviter

### accept-invitation
This event is posted when an invitation is accepted by a user.


## Ada Beans

### Beans

| Name           | Description                                                               |
|:---------------|:--------------------------------------------------------------------------|
|workspace|This bean allows to perform some general workspace actions|
|memberList|The list of workspace members.|
|inviteUser|The invitation bean.|
|workspaceMember|The workspace member bean.|

### Permissions

| Name           | Entity type  | Description                                                |
|:---------------|:-------------|:-----------------------------------------------------------|
|workspace-create|awa_workspace|Permission to create a workspace.|
|workspace-invite-user|awa_workspace|Permission to invite a user in the workspace.|
|workspace-delete-user|awa_workspace|Permission to delete a user from the workspace.|
|workspaces-create|awa_workspace||

### Configuration

| Name                      | Description                                                    |
|:--------------------------|:---------------------------------------------------------------|
|workspaces.permissions_list||
| |blog-create,wiki-space-create|
|workspaces.allow_workspace_create||
| |0|



## Data Model
![](images/awa_workspace_model.png)


