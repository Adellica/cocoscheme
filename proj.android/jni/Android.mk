LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := hellocpp_shared

LOCAL_MODULE_FILENAME := libhellocpp

LOCAL_SRC_FILES := hellocpp/main.cpp \
                   ../../Classes/AppDelegate.cpp \
                   ../../Classes/HelloWorldScene.cpp

LOCAL_C_INCLUDES := $(LOCAL_PATH)/../../Classes

# this is hacky and tricky. cocos2dx is built as a static lib, and
# both cocoscheme and hellocpp needs it. so we wrap it fully in
# cocoscheme and include that in hellocpp. that's why hellocpp needs
# the chicken sharedlib, and why we don't need cocos2dx here (it's
# included with cocoscheme)
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_WHOLE_STATIC_LIBRARIES := cocoscheme #cocos2dx_static cocoscheme

include $(BUILD_SHARED_LIBRARY)


include $(CLEAR_VARS)
LOCAL_MODULE := cocoscheme
LOCAL_SRC_FILES := cocoscheme.cpp
LOCAL_SHARED_LIBRARIES := chicken 	
LOCAL_CFLAGS := -DC_SHARED -Wno-write-strings
LOCAL_WHOLE_STATIC_LIBRARIES := cocos2dx_static # TODO make shared
include $(BUILD_STATIC_LIBRARY)


$(shell csc -t ../scm/bind/bind.scm -o ./jni/bind.c -J -include-path ../scm/bind)
include $(CLEAR_VARS)
LOCAL_MODULE := bind
LOCAL_SRC_FILES := bind.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)

$(shell csc -t ./bind.import.scm -o ./jni/bind.import.c)
include $(CLEAR_VARS)
LOCAL_MODULE := bind.import
LOCAL_SRC_FILES := bind.import.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)



$(shell csc -t ../scm/coops/coops-module.scm -o ./jni/coops.c -J -include-path ../scm/coops)
include $(CLEAR_VARS)
LOCAL_MODULE := coops
LOCAL_SRC_FILES := coops.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)

$(shell csc -t ./coops.import.scm -o ./jni/coops.import.c)
include $(CLEAR_VARS)
LOCAL_MODULE := coops.import
LOCAL_SRC_FILES := coops.import.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)


$(shell csc -t ../scm/bind/cplusplus-object.scm -o ./jni/cplusplus-object.c -J -include-path ../scm/coops)
include $(CLEAR_VARS)
LOCAL_MODULE := cplusplus-object
LOCAL_SRC_FILES := cplusplus-object.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)

$(shell csc -t ./cplusplus-object.import.scm -o ./jni/cplusplus-object.import.c)
include $(CLEAR_VARS)
LOCAL_MODULE := cplusplus-object.import
LOCAL_SRC_FILES := cplusplus-object.import.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)


$(shell csc -t ../scm/matchable/matchable.scm -o ./jni/matchable.c -J)
include $(CLEAR_VARS)
LOCAL_MODULE := matchable
LOCAL_SRC_FILES := matchable.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)

$(shell csc -t ./matchable.import.scm -o ./jni/matchable.import.c)
include $(CLEAR_VARS)
LOCAL_MODULE := matchable.import
LOCAL_SRC_FILES := matchable.import.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)


$(shell csc -t ../scm/record-variants/record-variants.scm -o ./jni/record-variants.c -J)
include $(CLEAR_VARS)
LOCAL_MODULE := record-variants
LOCAL_SRC_FILES := record-variants.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)

$(shell csc -t ./record-variants.import.scm -o ./jni/record-variants.import.c)
include $(CLEAR_VARS)
LOCAL_MODULE := record-variants.import
LOCAL_SRC_FILES := record-variants.import.c
LOCAL_SHARED_LIBRARIES := chicken
LOCAL_CFLAGS := -DC_SHARED
include $(BUILD_SHARED_LIBRARY)


$(call import-module,chickmunk)

$(call import-module,cocos2dx)
$(call import-module,chicken)
