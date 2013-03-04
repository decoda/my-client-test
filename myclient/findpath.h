#ifndef findpath_h__
#define findpath_h__

typedef std::list<Point2d> list_Point;

extern char mapdata[];
const int MAP_BLOCK_DISTANCE = 20;
const int MAP_BLOCK_HALF_DISTANCE = 10;

bool FindPath(Point2d start, Point2d end, list_Point &ret);
bool GetMovePath(const list_Point &pt_in, list_Point &pt_out);

void InitMapData();
void SetBlock(int x, int y);

int ComputeDirection(Point2d src, Point2d des);
Point2d LocalToScene(Point2d pt);

#endif // findpath_h__