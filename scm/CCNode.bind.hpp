
class CCObject {};
class CCAction {};

class CCArray : public CCObject
{
public:
  unsigned int count();
  CCObject* objectAtIndex(unsigned int index);
};


class CCPoint : public CCObject 
{
public:
  CCPoint(float x, float y); // coops supports only one constructor,
                             // this is the winner.
  void setPoint(float x, float y);
};

class CCSize : public CCObject 
{
public:
  CCSize(float w, float h);
};
class CCRect : public CCObject
{
public:
  CCRect(float x, float y, float width, float height);
};


class CCScene {} ;
// Nodes and such:

class CCDirector {
public:
  virtual float getSecondsPerFrame() = 0;
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

// layer
class CCLayer : public CCNode // : TODO
{
public:
  static CCLayer* create();
};

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
  virtual void setOpacity(unsigned char var);
  //virtual unsigned char getOpacity();
  virtual void setTextureRect( CCRect &r); //FIX: this is
                                                //actually CCRect &r,
                                                //but bind fails
  //protected virtual void setTextureCoord(CCRect &r);
};

class CCLabelTTF : public CCSprite
{
public:
  static CCLabelTTF* create(const char* string, const char* fontName, float fontsize);
  void setString(const char* label);
};



// actions


class CCRotateBy // : public CCActionInterval
{
public:
  static CCRotateBy* create(float duration, float fDeltaAngle);
};

class CCMoveBy // TODO
{
public:
  static CCMoveBy * create(float duration, const CCPoint &position);
};

class CCFlipX // : TODO
{ public static CCFlipX* create(bool x); };

class CCFlipY // : TODO
{ public static CCFlipY* create(bool y); };

// events

class CCTouch : public CCObject
{
public:
  // CCPoint getLocation();
};

// drawing

// does not work:
void ccDrawColor4F(float r, float g, float b, float a );
void ccDrawColor4B(unsigned char r, unsigned char g, unsigned char b, unsigned char a );

void ccDrawLine(const CCPoint &origin, const CCPoint &destination);
void ccDrawCircle(const CCPoint& center, float radius, float angle, unsigned int segments, bool drawLineToCenter);

// unable to map:
//void ccDrawPoly( const CCPoint *poli, unsigned int numberOfPoints, bool closePolygon );
// void ccDrawSolidPoly( const CCPoint *poli, unsigned int
// numberOfPoints, ccColor4F color );

// natively defined:
// void ccDrawSolidRect( CCPoint* origin, CCPoint* destination, float r, float g, float b, float a )

