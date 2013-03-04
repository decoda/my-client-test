#include "StdAfx.h"
#include "Timer.h"
#include "MainPlayer.h"

initialiseSingleton(CTimer);
CTimer::CTimer()
{
	m_TimeStart = GetTick();
	m_TimeNow = m_TimeStart;
}

CTimer::~CTimer()
{
	
}

void CTimer::Run()
{
	m_TimeNow = GetTick();
}

uint32 CTimer::GetTick()
{
	static LARGE_INTEGER perfCounterFreq = {{0, 0}};
	if(perfCounterFreq.QuadPart == 0){
		if(QueryPerformanceFrequency(&perfCounterFreq) == FALSE){
			//looks like the system does not support high resolution tick counter
			return GetTickCount();
		}
	}
	LARGE_INTEGER ticks;
	if(QueryPerformanceCounter(&ticks) == FALSE){
		return GetTickCount();
	}
	return uint32((ticks.QuadPart * 1000) / perfCounterFreq.QuadPart);
}


bool CTimerThread::run()
{
	while (true)
	{
		if(GetThreadState() == THREADSTATE_TERMINATE)
			break;

		sTimer.Run();

		::Sleep(1);
	}
	return true;
}
