#ifndef StdAfx_h__
#define StdAfx_h__

#include <cstdlib>
#include <cstdio>

#include <cstdarg>
#include <ctime>
#include <cmath>
#include <cerrno>

#pragma warning(disable:4996)
#define _CRT_SECURE_NO_DEPRECATE 1
#define _CRT_SECURE_COPP_OVERLOAD_STANDARD_NAMES 1
#pragma warning(disable:4251)

#ifndef STRSAFE_NO_DEPRECATE
#define STRSAFE_NO_DEPRECATE
#endif

#define WIN32_LEAN_AND_MEAN
//#  define _WIN32_WINNT 0x0500
#define NOMINMAX
#include <windows.h>
#undef NOMINMAX

#include <set>
#include <list>
#include <string>
#include <map>
#include <queue>
#include <sstream>
#include <algorithm>
#include <cstring>
#include <climits>
#include <hash_map>

#include <tchar.h>
#include <strsafe.h>
#include <memory>

#define _USE_MATH_DEFINES
#include <math.h>

#include <Winsock2.h>
#pragma comment(lib, "Ws2_32.lib")

#include "common.h"
#include "Timer.h"
#include "fsa.h"
#include "WorldPacket.h"

#endif // StdAfx_h__
