////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// STL A* Search implementation
// (C)2001 Justin Heyes-Jones
//
// Finding a path on a simple grid maze
// This shows how to do shortest path finding using A*

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
#include "StdAfx.h"
#include "findpath.h"
#include "stlastar.h" // See header for copyright and usage information

#include <iostream>
#include <stdio.h>

#define DEBUG_LISTS 0
#define DEBUG_LIST_LENGTHS_ONLY 0
#define BLOCK_POS 10

using namespace std;

// Global data

// The world map

const int MAP_WIDTH = 40;
const int MAP_HEIGHT = 30;

char mapdata[ MAP_WIDTH * MAP_HEIGHT ];

// map helper functions

int GetMap( int x, int y )
{

	if( x < 0 ||
	    x >= MAP_WIDTH ||
		 y < 0 ||
		 y >= MAP_HEIGHT
	  )
	{
		return BLOCK_POS;	 
	}

	return mapdata[(y*MAP_WIDTH)+x];
}



// Definitions

class MapSearchNode
{
public:
	unsigned int x;	 // the (x,y) positions of the node
	unsigned int y;	
	
	MapSearchNode() { x = y = 0; }
	MapSearchNode( unsigned int px, unsigned int py ) { x=px; y=py; }

	float GoalDistanceEstimate( MapSearchNode &nodeGoal );
	bool IsGoal( MapSearchNode &nodeGoal );
	bool GetSuccessors( AStarSearch<MapSearchNode> *astarsearch, MapSearchNode *parent_node );
	float GetCost( MapSearchNode &successor );
	bool IsSameState( MapSearchNode &rhs );
};

bool MapSearchNode::IsSameState( MapSearchNode &rhs )
{

	// same state in a maze search is simply when (x,y) are the same
	if( (x == rhs.x) &&
		(y == rhs.y) )
	{
		return true;
	}
	else
	{
		return false;
	}

}

// Here's the heuristic function that estimates the distance from a Node
// to the Goal. 

float MapSearchNode::GoalDistanceEstimate( MapSearchNode &nodeGoal )
{
	float xd = abs( ( (float)x - (float)nodeGoal.x ) );
	float yd = abs( ( (float)y - (float)nodeGoal.y) );

	return xd + yd;

}

bool MapSearchNode::IsGoal( MapSearchNode &nodeGoal )
{

	if( (x == nodeGoal.x) &&
		(y == nodeGoal.y) )
	{
		return true;
	}

	return false;
}

// This generates the successors to the given Node. It uses a helper function called
// AddSuccessor to give the successors to the AStar class. The A* specific initialisation
// is done for each node internally, so here you just set the state information that
// is specific to the application
bool MapSearchNode::GetSuccessors( AStarSearch<MapSearchNode> *astarsearch, MapSearchNode *parent_node )
{

	int parent_x = -1; 
	int parent_y = -1; 

	if( parent_node )
	{
		parent_x = parent_node->x;
		parent_y = parent_node->y;
	}
	

	MapSearchNode NewNode;

	// push each possible move except allowing the search to go backwards

	const int xy[8][2] = 
	{
		{-1,  0},
		{-1, -1},
		{ 0, -1},
		{ 1, -1},
		{ 1,  0},
		{ 1,  1},
		{ 0,  1},
		{-1,  1}
	};

	for (int i = 0; i < 8; ++i)
	{
		int x1 = x + xy[i][0];
		int y1 = y + xy[i][1];
		if(((GetMap(x1, y1) < BLOCK_POS) && !(parent_x == x1 && parent_y == y1)))
		{
			NewNode = MapSearchNode(x1, y1);
			astarsearch->AddSuccessor( NewNode );
		}
	}

	return true;
}

// given this node, what does it cost to move to successor. In the case
// of our map the answer is the map terrain value at this node since that is 
// conceptually where we're moving

float MapSearchNode::GetCost( MapSearchNode &successor )
{
	// return (float) GetMap( x, y );
	if (successor.x == x || successor.y == y)
		return 1.0f;
	else
		return 1.41421f;
}

bool FindPath( Point2d start, Point2d end, list_Point &ret )
{
	static AStarSearch<MapSearchNode> astarsearch;

	// Create a start state
	MapSearchNode nodeStart;
	nodeStart.x = start.x;
	nodeStart.y = start.y;

	// Define the goal state
	MapSearchNode nodeEnd;
	nodeEnd.x = end.x;		
	nodeEnd.y = end.y;
		
	astarsearch.SetStartAndGoalStates( nodeStart, nodeEnd );

	unsigned int SearchState;
	unsigned int SearchSteps = 0;
	do
	{
		SearchState = astarsearch.SearchStep();
		SearchSteps++;
	}
	while( SearchState == AStarSearch<MapSearchNode>::SEARCH_STATE_SEARCHING );

	if( SearchState == AStarSearch<MapSearchNode>::SEARCH_STATE_SUCCEEDED )
	{
		MapSearchNode *node = astarsearch.GetSolutionStart();
		for( ;; )
		{
			node = astarsearch.GetSolutionNext();

			if( !node )
			{
				break;
			}

			Point2d pt;
			pt.x = node->x;
			pt.y = node->y;
			ret.push_back(pt);

			astarsearch.FreeSolutionNodes();
		};
		//printf("searchsteps %d path %d\n", SearchSteps, ret.size());
		return true;
	}
	else if( SearchState == AStarSearch<MapSearchNode>::SEARCH_STATE_FAILED ) 
	{
		return false;
	}

	return false;
}

void InitMapData()
{
	memset(mapdata, 1, sizeof(mapdata));
// 	char block[11][2] = {{7,6},{8,6},{9,6},{10,6},{11,6},{12,6},{7,7},{7,8},{7,9},{7,10},{7,11}};
// 	for (int i = 0; i < 11; ++i)
// 	{
// 		int x = block[i][0];
// 		int y = block[i][1];
// 		mapdata[40*y + x] = BLOCK_POS;
// 	}
}

void SetBlock(int x, int y)
{
	int index = y * MAP_WIDTH + x;
	char *val = mapdata + index;
	if (*val == 1)
		*val = BLOCK_POS;
	else if (*val == BLOCK_POS)
		*val = 1;
}

bool GetMovePath(const list_Point &pt_in, list_Point &pt_out)
{
	bool bFront = true;
	list_Point::const_iterator it = pt_in.begin();
	while (true)
	{
		Point2d ptCur = *it;
		list_Point::const_iterator itNext = ++it;
		if (itNext == pt_in.end())
			break;

		Point2d ptNext = *itNext;
		if (!bFront)
		{
			list_Point::const_iterator itNextNext = ++itNext;
			if (itNextNext != pt_in.end())
			{
				Point2d ptNextNext = *itNextNext;
				if (ptNext.x - ptCur.x == ptNextNext.x - ptNext.x && 
					ptNext.y - ptCur.y == ptNextNext.y - ptNext.y)
					continue;
			}
		}
		bFront = false;
		Point2d ptOut;
		if (ptCur.x < ptNext.x && ptCur.y < ptNext.y)
		{
			ptOut.x = ptNext.x * MAP_BLOCK_DISTANCE;
			ptOut.y = ptNext.y * MAP_BLOCK_DISTANCE;
			pt_out.push_back(ptOut);
		}
		else if(ptCur.x > ptNext.x && ptCur.y < ptNext.y)
		{
			ptOut.x = ptCur.x * MAP_BLOCK_DISTANCE;
			ptOut.y = ptNext.y * MAP_BLOCK_DISTANCE;
			pt_out.push_back(ptOut);
		}
		else if(ptCur.x > ptNext.x && ptCur.y > ptNext.y)
		{
			ptOut.x = ptCur.x * MAP_BLOCK_DISTANCE;
			ptOut.y = ptCur.y * MAP_BLOCK_DISTANCE;
			pt_out.push_back(ptOut);
		}
		else if(ptCur.x < ptNext.x && ptCur.y > ptNext.y)
		{
			ptOut.x = ptNext.x * MAP_BLOCK_DISTANCE;
			ptOut.y = ptCur.y * MAP_BLOCK_DISTANCE;
			pt_out.push_back(ptOut);
		}

		ptOut.x = ptNext.x * MAP_BLOCK_DISTANCE + MAP_BLOCK_HALF_DISTANCE;
		ptOut.y = ptNext.y * MAP_BLOCK_DISTANCE + MAP_BLOCK_HALF_DISTANCE;
		pt_out.push_back(ptOut);
	}
	return true;
}

int ComputeDirection(Point2d src, Point2d des)
{
	double dy, dx, k;
	int direction = DIRECTION_BOTTOM_RIGHT;
	dy = des.y - src.y;
	dx = des.x - src.x;
	if (dx == 0) {
		return (dy >= 0) ? DIRECTION_BOTTOM : DIRECTION_TOP;
	} else if (dy == 0) {
		return (dx >= 0) ? DIRECTION_RIGHT : DIRECTION_LEFT;
	}
	static const double k1 = ::tan(M_PI / 8);
	static const double k2 = 3 * k1;
	k = ::abs(dy / dx);
	if (k >= k2) {
		if (dy > 0)
			direction = DIRECTION_BOTTOM;
		else
			direction = DIRECTION_TOP;
	} else if (k <= k1) {
		if (dx > 0)
			direction = DIRECTION_RIGHT;
		else
			direction = DIRECTION_LEFT;
	} else if (dy > 0) {
		if (dx > 0)
			direction = DIRECTION_BOTTOM_RIGHT;
		else
			direction = DIRECTION_BOTTOM_LEFT;
	} else {
		if (dx > 0)
			direction = DIRECTION_TOP_RIGHT;
		else
			direction = DIRECTION_TOP_LEFT;
	}
	return direction;
}

Point2d LocalToScene(Point2d pt)
{
	return Point2d(pt.x / MAP_BLOCK_DISTANCE, pt.y / MAP_BLOCK_DISTANCE);
}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////
