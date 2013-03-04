#include "StdAfx.h"
#include "ThreadBase.h"

CThread::CThread() : ThreadBase()
{
	ThreadState.SetVal(THREADSTATE_AWAITING);
	start_time  = 0;
}

CThread::~CThread()
{

}

bool CThread::run()
{
	return false;
}

void CThread::OnShutdown()
{
	SetThreadState(THREADSTATE_TERMINATE);
}