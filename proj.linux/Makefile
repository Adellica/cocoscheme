CC      = gcc
CXX     = g++
TARGET	= HelloCpp
CCFLAGS = -Wall
CXXFLAGS = -Wall
VISIBILITY = 

COCOS2DX_HOME = /home/klm/opt/cocos2d-2.0-x-2.0.3/
COCOS2DX_PATH = $(COCOS2DX_HOME)/cocos2dx

INCLUDES =  -I../ \
			-I../Classes \
			-I$(COCOS2DX_PATH) \
			-I$(COCOS2DX_PATH)/platform/third_party/linux \
			-I$(COCOS2DX_PATH)/platform/third_party/linux/libfreetype2 \
			-I$(COCOS2DX_PATH)/cocoa \
			-I$(COCOS2DX_PATH)/include \
			-I$(COCOS2DX_PATH)/platform \
			-I$(COCOS2DX_PATH)/platform/linux \
			-I$(COCOS2DX_PATH)/platform/third_party/linux/glew-1.7.0/glew-1.7.0/include/ \
			-I$(COCOS2DX_PATH)/kazmath/include \
			-I$(COCOS2DX_PATH)/platform/third_party/linux/libxml2 \
			-I$(COCOS2DX_PATH)/platform/third_party/linux/libjpeg  \


DEFINES = -DLINUX

OBJECTS = ./main.o \
        ../Classes/AppDelegate.o \
        ../Classes/HelloWorldScene.o \
#        ../Classes/cocos2dx_extension/Extensions/CCLayerPanZoom/CCLayerPanZoom.o

LBITS := $(shell getconf LONG_BIT)
ifeq ($(LBITS),64)
STATICLIBS_DIR = $(COCOS2DX_PATH)/platform/third_party/linux/libraries/lib64
else
STATICLIBS_DIR = $(COCOS2DX_PATH)/platform/third_party/linux/libraries
endif
STATICLIBS = 
STATICLIBS = $(STATICLIBS_DIR)/libfreetype.a \
				$(STATICLIBS_DIR)/libxml2.a \
				$(STATICLIBS_DIR)/libpng.a \
				$(STATICLIBS_DIR)/libjpeg.a \
				$(STATICLIBS_DIR)/libtiff.a \
#				$(STATICLIBS_DIR)/libGLEW.a \

SHAREDLIBS = -lglfw -lcurl ./cocoscheme.so
SHAREDLIBS += -Wl,-rpath,$(COCOS2DX_PATH)/platform/third_party/linux/glew-1.7.0/glew-1.7.0/lib
SHAREDLIBS += -L$(COCOS2DX_PATH)/platform/third_party/linux/glew-1.7.0/glew-1.7.0/lib -lGLEW

#$(shell ../../build-linux.sh $<)

BIN_DIR_ROOT=bin
BIN_DIR = $(BIN_DIR_ROOT)

debug: BIN_DIR = $(BIN_DIR_ROOT)/debug
debug: CCFLAGS += -g3 -O0
debug: CXXFLAGS += -g3 -O0
debug: SHAREDLIBS += -L$(COCOS2DX_HOME)/lib/linux/Debug -lcocos2d -lrt -lz -lcocosdenshion
debug: SHAREDLIBS += -Wl,-rpath,$(COCOS2DX_HOME)/lib/linux/Debug/
debug: DEFINES += -DDEBUG
debug: $(TARGET)

release: BIN_DIR = $(BIN_DIR_ROOT)/release
release: CCFLAGS += -O3
release: CXXFLAGS += -O3
release: SHAREDLIBS += -L$(COCOS2DX_HOME)/lib/linux/Release -lcocos2d -lrt -lz -lcocosdenshion
release: SHAREDLIBS += -Wl,-rpath,$(COCOS2DX_HOME)/lib/linux/Release/
release: DEFINES += -DNDEBUG
release: $(TARGET)

####### Build rules
$(TARGET): $(OBJECTS) ./cocoscheme.so
	mkdir -p $(BIN_DIR)
	$(CXX) $(CXXFLAGS) $(INCLUDES) $(DEFINES) $(OBJECTS) -o $(BIN_DIR)/$(TARGET) $(SHAREDLIBS) $(STATICLIBS)

####### Compile
./cocoscheme.so: ../scm/cocoscheme.scm
	csc -s -c++ ../scm/cocoscheme.scm -o ./cocoscheme.so -C -DLINUX $(INCLUDES) -include-path ../scm

../scm/cocoscheme.scm: ../scm/cocoscheme-*.scm
#	touch ../scm/cocoscheme.scm

%.o: %.cpp
	$(CXX) $(CXXFLAGS) $(INCLUDES) $(DEFINES) $(VISIBILITY) -c $< -o $@

%.o: %.c
	$(CC) $(CCFLAGS) $(INCLUDES) $(DEFINES) $(VISIBILITY) -c $< -o $@

clean: 
	rm -f $(OBJECTS) $(TARGET) core
