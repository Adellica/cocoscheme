
class CCObject {};
class CCAction {};

class CCRotateBy // : public CCActionInterval
{
public:
  static CCRotateBy* create(float duration, float fDeltaAngle);
};

class CCArray 
{
public:
  unsigned int count();
  CCObject* objectAtIndex(unsigned int index);
};

class CCNode // : public CCObject TODO
{
public:
  virtual void addChild(CCNode * child);

  float getPositionX(void);
  float getPositionY(void);
  
  virtual float getRotation();
  virtual CCArray* getChildren();

  void setPosition(float x, float y);

  CCAction* runAction(CCAction* action);
  void stopAllActions(void);
};

class CCLayer : public CCNode {} ; // TODO
class CCMenu : public CCLayer {} ; // TODO

class CCMenuItem : public CCNode {} ; // TODO
class CCMenuItemSprite : public CCMenuItem {} ; // TODO
class CCMenuItemImage : public CCMenuItemSprite
{
};


class CCSprite : public CCNode
{
public:
  static CCSprite* create(const char *pszFileName);
};

