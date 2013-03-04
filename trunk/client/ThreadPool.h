#ifndef ThreadPool_h__
#define ThreadPool_h__

#include "common.h"

class ThreadController
{
public:
	HANDLE hThread;
	uint32 thread_id;

	void Setup(HANDLE h)
	{
		hThread = h;
		// whoops! GetThreadId is for windows 2003 and up only! :<		 - Burlex
		//thread_id = (uint32)GetThreadId(h);
	}

	void Suspend()
	{
		// We can't be suspended by someone else. That is a big-no-no and will lead to crashes.
		ASSERT(GetCurrentThreadId() == thread_id);
		SuspendThread(hThread);
	}

	void Resume()
	{
		// This SHOULD be called by someone else.
		ASSERT(GetCurrentThreadId() != thread_id);
		if(ResumeThread(hThread) == DWORD(-1))
		{
			DWORD le = GetLastError();
			printf("lasterror: %u\n", le);
		}
	}

	void Join()
	{
		WaitForSingleObject(hThread, INFINITE);
	}

	uint32 GetId() { return thread_id; }
};

struct Thread
{
	ThreadBase* ExecutionTarget;
	ThreadController ControlInterface;
	Mutex SetupMutex;
	bool DeleteAfterExit;
};

typedef std::set<Thread*> ThreadSet;

class CThreadPool
{
	int GetNumCpus();

	uint32 _threadsRequestedSinceLastCheck;
	uint32 _threadsFreedSinceLastCheck;
	uint32 _threadsExitedSinceLastCheck;
	uint32 _threadsToExit;
	int32 _threadsEaten;
	Mutex _mutex;

	ThreadSet m_activeThreads;
	ThreadSet m_freeThreads;

public:
	CThreadPool();

	// call every 2 minutes or so.
	void IntegrityCheck();

	// call at startup
	void Startup();

	// shutdown all threads
	void Shutdown();

	// return true - suspend ourselves, and wait for a future task.
	// return false - exit, we're shutting down or no longer needed.
	bool ThreadExit(Thread* t);

	// creates a thread, returns a handle to it.
	Thread* StartThread(ThreadBase* ExecutionTarget);

	// grabs/spawns a thread, and tells it to execute a task.
	void ExecuteTask(ThreadBase* ExecutionTarget);

	// prints some neat debug stats
	void ShowStats();

	// kills x free threads
	void KillFreeThreads(uint32 count);

	// resets the gobble counter
	inline void Gobble() { _threadsEaten = (int32)m_freeThreads.size(); }

	// gets active thread count
	inline uint32 GetActiveThreadCount() { return (uint32)m_activeThreads.size(); }

	// gets free thread count
	inline uint32 GetFreeThreadCount() { return (uint32)m_freeThreads.size(); }
};

extern CThreadPool ThreadPool;

#endif // ThreadPool_h__