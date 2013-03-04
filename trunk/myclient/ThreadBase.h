#ifndef ThreadBase_h__
#define ThreadBase_h__

class ThreadBase
{
public:
	ThreadBase() {}
	virtual ~ThreadBase() {}
	virtual bool run() = 0;
	virtual void OnShutdown() {}

	HANDLE THREAD_HANDLE;
};

enum CThreadState
{
	THREADSTATE_TERMINATE = 0,
	THREADSTATE_PAUSED	  = 1,
	THREADSTATE_SLEEPING  = 2,
	THREADSTATE_BUSY	  = 3,
	THREADSTATE_AWAITING  = 4,
};

class CThread : public ThreadBase
{
public:
	CThread();
	~CThread();

	inline void SetThreadState(CThreadState thread_state) { ThreadState.SetVal(thread_state); }
	inline CThreadState GetThreadState()
	{
		unsigned long val = ThreadState.GetVal();
		return static_cast<CThreadState>(val);
	}
	int GetThreadId() { return ThreadId; }
	time_t GetStartTime() { return start_time; }
	virtual bool run();
	virtual void OnShutdown();

protected:
	CThread & operator=(CThread & other)
	{
		this->start_time = other.start_time;
		this->ThreadId = other.ThreadId;
		this->ThreadState.SetVal(other.ThreadState.GetVal());
		return *this;
	}

	Threading::AtomicCounter ThreadState;
	time_t start_time;
	int ThreadId;
};

#endif // ThreadBase_h__