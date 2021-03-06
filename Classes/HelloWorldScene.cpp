#include "HelloWorldScene.h"
#include <stdio.h>

USING_NS_CC;

extern "C" void cs_init();

extern "C" void c_foo(CCNode*, CCDirector*);
extern "C" void c_draw();
extern "C" void c_callback(CCObject* sender);
extern "C" void c_touch_begin(CCTouch*);
extern "C" void c_touch_moved(CCTouch*);
extern "C" void c_touch_ended(CCTouch*);

CCScene* HelloWorld::scene()
{
    // 'scene' is an autorelease object
    CCScene *scene = CCScene::create();
    
    // 'layer' is an autorelease object
    HelloWorld *layer = HelloWorld::create();

    // add layer as a child to scene
    scene->addChild(layer);

    // return the scene
    return scene;
}

void HelloWorld::draw()
{
  //c_draw();
}

class SpaceLayer : public CCLayer
{
public:
  virtual void draw();
  CREATE_FUNC(SpaceLayer);
};

void SpaceLayer::draw() 
{
  //printf("drawing spacelayer'
  c_draw();
}

static CCSprite* sp;
static SpaceLayer* sl;
void HelloWorld::update(float dt) 
{
  // FIX: cleanup! updates and draws of scene and spacelayer
  c_foo(sl, CCDirector::sharedDirector());
}


// on "init" you need to initialize your instance
bool HelloWorld::init()
{
  fprintf(stderr, "scene.cpp: +++++++++++++++++++ HelloWorld cs_init()\n");
  cs_init();
  fprintf(stderr, "scene.cpp: +++++++++++++++++++ HelloWorld cs_init done()\n");
  
    //////////////////////////////
    // 1. super init first
    if ( !CCLayer::init() )
    {
        return false;
    }
    fprintf(stderr, "scene.cpp: CCLayer::init() ok\n");
    CCSize visibleSize = CCDirector::sharedDirector()->getVisibleSize();
    CCPoint origin = CCDirector::sharedDirector()->getVisibleOrigin();


    CCDirector::sharedDirector()->setDisplayStats(0);
    /////////////////////////////
    // 2. add a menu item with "X" image, which is clicked to quit the program
    //    you may modify it.

    // add a "close" icon to exit the progress. it's an autorelease object
    CCMenuItemImage *pCloseItem = CCMenuItemImage::create(
                                        "CloseNormal.png",
                                        NULL,
                                        this,
                                        menu_selector(HelloWorld::menuCloseCallback));
        
    pCloseItem->setPosition(ccp(origin.x + visibleSize.width - pCloseItem->getContentSize().width/2 ,
                                origin.y + pCloseItem->getContentSize().height/2));

    pCloseItem->runAction(CCRepeatForever::create(CCRotateBy::create(16, 360)));
    

    sl = SpaceLayer::create();
    this->addChild(sl, 1);
    // create menu, it's an autorelease object
    CCMenu* pMenu = CCMenu::create(pCloseItem, NULL);
    pMenu->setPosition(CCPointZero);
    this->addChild(pMenu, 1);

    // sp = CCSprite::create("ground/G000M800.png");
    // sp->setPosition(ccp(100, 100));
    // //    this->addChild(sp, 0);

    // CCSprite *sp2 = CCSprite::create("ground/G000M800.png");
    // sp2->setPosition(ccp(229, 100));
    // //    this->addChild(sp2, 0);

    //    sp2->runAction(CCRepeatForever::create(CCRotateBy::create(5, 90)));


    /////////////////////////////
    // 3. add your codes below...

    // add a label shows "Hello World"
    // create and initialize a label
    //CCLabelTTF* pLabel = CCLabelTTF::create("Hello World", "Arial", 24);

    // // position the label on the center of the screen
    // pLabel->setPosition(ccp(origin.x + visibleSize.width/2,
    //                         origin.y + visibleSize.height - pLabel->getContentSize().height));

    // // add the label as a child to this layer
    // this->addChild(pLabel, 1);

    // add "HelloWorld" splash screen"
    //CCSprite* pSprite = CCSprite::create("HelloWorld.png");
    // position the sprite on the center of the screen
    //pSprite->setPosition(ccp(visibleSize.width/2 + origin.x, visibleSize.height/2 + origin.y));

    // add the sprite as a child to this layer
    //this->addChild(pSprite, 0);
    
    // enable standard touch
    this->setTouchEnabled(true);

    // make sure we call our scheme callback
    this->scheduleUpdate();

    return true;
}

void HelloWorld::menuCloseCallback(CCObject* pSender)
{
  c_callback(pSender);
  //CCDirector::sharedDirector()->end();
  //sp->runAction(CCRotateBy::create(2, 45));
// #if (CC_TARGET_PLATFORM == CC_PLATFORM_IOS)
//     exit(0);
// #endif
}

void HelloWorld::ccTouchesBegan(CCSet *pTouches, CCEvent *pEvent)
{
    CCTouch* touch = (CCTouch*)(* pTouches->begin());
    CCPoint pos = touch->getLocation();

    c_touch_begin(touch);
}

void HelloWorld::ccTouchesMoved(CCSet *pTouches, CCEvent *pEvent)
{
    CCTouch* touch = (CCTouch*)(* pTouches->begin());

    c_touch_moved(touch);
}

void HelloWorld::ccTouchesEnded(CCSet *pTouches, CCEvent *pEvent)
{
    CCTouch* touch = (CCTouch*)(* pTouches->begin());

    c_touch_ended(touch);
}
