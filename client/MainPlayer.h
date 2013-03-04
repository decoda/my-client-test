#ifndef MainPlayer_h__
#define MainPlayer_h__

#include "player.h"

class MySocket;

class MainPlayer : public Player, public Singleton<MainPlayer>
{
public:
	MainPlayer(void);
	~MainPlayer(void);

	static void* operator new(std::size_t);
	static void operator delete(void*, std::size_t);

public:
	virtual void Run(uint32 elapse);
	virtual bool MoveTo(int x, int y);
	virtual void OnMoveTo(int x, int y);
	virtual void SetSelect(int x, int y);
public:
	void LoginServer(const char* szServerAddr, uint32 nServerPort);
	void OnSocketEvent(SOCKET sock, int err, int evt);

	bool IsLogin() const { return m_ID != 0; }
	bool CanSend() const;
	void SendPacket(const WorldPacket &packet);

protected:
	void HandleLogin(WorldPacket &pack);
	void HandlePath(WorldPacket &pack);
	void HandleAddObj(WorldPacket &pack);
	void HandleRemoveObj(WorldPacket &pack);

private:
	void OnDisConnect();

private:
	MySocket *m_pSocket;
	int32 m_nStepError;
};

#define sPlayerMain MainPlayer::getSingleton()

#endif // MainPlayer_h__

