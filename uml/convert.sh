#!/bin/sh
# Usage: convert.sh target-dir
if test $# -ne 1; then
   echo "Usage: convert.sh destination-directory"
   exit 1
fi
TARGET=$1
convert -flatten VoteBeans.png $TARGET/awa_votes_bean.png
convert -flatten VoteModel.png $TARGET/awa_votes_model.png
convert -flatten BlogBeans.png $TARGET/awa_blogs_bean.png
convert -flatten BlogModel.png $TARGET/awa_blogs_model.png
convert -flatten CommentBean.png $TARGET/awa_comments_bean.png
convert -flatten CommentModel.png $TARGET/awa_comments_model.png
# CommentsModule.png
convert -flatten CounterBeans.png $TARGET/awa_counters_bean.png
convert -flatten CounterModel.png $TARGET/awa_counters_model.png
convert -flatten ImageModel.png $TARGET/awa_images_model.png
# ImagesModule.png
convert -flatten JobModel.png $TARGET/awa_jobs_model.png
convert -flatten QuestionBeans.png $TARGET/awa_questions_bean.png
convert -flatten QuestionModel.png $TARGET/awa_questions_model.png
convert -flatten SettingsModel.png $TARGET/awa_settings_model.png
convert -flatten SettingsModule.png $TARGET/awa_settings_module.png
convert -flatten StorageBeans.png $TARGET/awa_storages_bean.png
convert -flatten StorageModel.png $TARGET/awa_storages_model.png
convert -flatten StorageModule.png $TARGET/awa_storages_module.png

convert -flatten WikiBeans.png $TARGET/awa_wikis_bean.png
convert -flatten WikiModel.png $TARGET/awa_wikis_model.png

convert -flatten TagsModel.png $TARGET/awa_tags_model.png

convert -flatten OAuthModel.png $TARGET/awa_oauth_model.png
convert -flatten CountryModel.png $TARGET/awa_country_model.png
convert -flatten EventModel.png $TARGET/awa_event_model.png

convert -flatten MailModel.png $TARGET/awa_mail_model.png

convert -flatten PermissionModel.png $TARGET/awa_permission_model.png

convert -flatten UserModel.png $TARGET/awa_user_model.png
convert -flatten WorkspaceModel.png $TARGET/awa_workspace_model.png

#convert -flatten NotificationModel.png $TARGET/awa_notifications_model.png

# AWACollaboration.png
# AWAInitialisation.png
# AWAInitialisati.png
# BeanModel.png
# ClassDiagram.png
# Conventions.png
# CountriesModule.png
# CreateWikiSpace.png
# EventService.png
# MailModule.png
# Model.png
# Modul.png
# Overview.png
# Requestflow.png
# SequenceDiagram.png
# UserModule.png
