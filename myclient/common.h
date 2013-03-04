#ifndef common_h__
#define common_h__

#ifdef _DEBUG
#include <assert.h>
#define ASSERT(x) assert(x)
#define DEBUG(...) do{ printf(__VA_ARGS__); printf("\n"); } while(0)
#else
#define ASSERT(x) void
#define DEBUG(...) void
#endif // _DEBUG

#pragma warning(disable:4244)

typedef signed __int64 int64;
typedef signed __int32 int32;
typedef signed __int16 int16;
typedef signed __int8 int8;

typedef unsigned __int64 uint64;
typedef unsigned __int32 uint32;
typedef unsigned __int16 uint16;
typedef unsigned __int8 uint8;

//IP地址的字符最大长度
#define IP_SIZE			24
#define WM_SOCKET		WM_USER + 1

#ifndef COUNT_OF
#define COUNT_OF(array) (sizeof(array) / sizeof((array)[0]))
#endif

// used for creating values for respawn for example
#define MAKE_PAIR64(l, h)  uint64( uint32(l) | ( uint64(h) << 32 ) )
#define PAIR64_HIPART(x)   (uint32)((uint64(x) >> 32) & UI64LIT(0x00000000FFFFFFFF))
#define PAIR64_LOPART(x)   (uint32)(uint64(x)         & UI64LIT(0x00000000FFFFFFFF))

#define MAKE_PAIR32(l, h)  uint32( uint16(l) | ( uint32(h) << 16 ) )
#define PAIR32_HIPART(x)   (uint16)((uint32(x) >> 16) & 0x0000FFFF)
#define PAIR32_LOPART(x)   (uint16)(uint32(x)         & 0x0000FFFF)

// 
#define PLAYER_SIZE_X 5
#define PLAYER_SIZE_Y 5

// 2d点
typedef struct _Point2d
{
	_Point2d(int _x = 0, int _y = 0) :x(_x), y(_y) {}
	_Point2d(const _Point2d& pt)
	{
		x = pt.x;
		y = pt.y;
	}
	int x;
	int y;
}Point2d;

// 方向
enum
{
	DIRECTION_BOTTOM_RIGHT	= 0x0,
	DIRECTION_BOTTOM_LEFT	= 0x1,
	DIRECTION_TOP_LEFT		= 0x2,
	DIRECTION_TOP_RIGHT		= 0x3,
	DIRECTION_BOTTOM		= 0x4,
	DIRECTION_LEFT			= 0x5,
	DIRECTION_TOP			= 0x6,
	DIRECTION_RIGHT			= 0x7
};


#include "Singleton.h"
#include "Mutex.h"
#include "Atomic.h"
#include "ThreadBase.h"
#include "ThreadPool.h"

#endif // common_h__