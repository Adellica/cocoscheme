
(bind-file "/home/klm/projects/cocos2d/HelloWorld/scm/CCNode.bind.hpp")


(bind* "
void ccDrawSolidPoly(float* points, unsigned int numberOfPoints, float r, float g, float b, float a)
{
 CCPoint p [ 32 ];
 ccColor4F color = {r, g, b, a};         
 for(int i = 0 ; i < MIN(numberOfPoints, 32) ; i++ ) {
    p [i].x = points [ i * 2 + 0];
    p [i].y = points [ i * 2 + 1];
 }
 ccDrawSolidPoly (p, MIN (numberOfPoints, 32), color);
}")

(bind* "
void ccDrawSolidRect( CCPoint* origin, CCPoint* destination, float r, float g, float b, float a )
{
  ccColor4F color = {r, g, b, a};
  ccDrawSolidRect(*origin, *destination, color );
}")
