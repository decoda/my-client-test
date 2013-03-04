#ifndef MySocket_h__
#define MySocket_h__

#include "BufferStream.h"
#include "WorldPacket.h"

typedef std::deque<WorldPacket> que_Pack;

class MySocket
{
	friend class MainPlayer;
public:
	MySocket();
	MySocket(const char* host, uint32 port);
	virtual ~MySocket ();

public:
	bool IsValid() const
	{
		return m_SocketID != INVALID_SOCKET;
	}
	SOCKET GetSocket()const
	{
		return m_SocketID;
	}
	uint32 Available() const;
	bool SetLinger(uint32 lingertime);
	bool SetNonBlocking(bool on = true);
	bool SetNonSendBuf(bool on = true);
	bool SetNonRecvBuf(bool on = true);

	bool Create();
	bool Connect();
	bool Connect(const char* host, uint32 port);
	bool IsConnect() const { return m_bConnect; }
	void OnConnect() { m_bConnect = true; }
	int32 Send(const WorldPacket &packet);
	int32 Send(const void* buff, int32 len);
	int32 Send();
	int32 Receive();
	void Close();


private:
	bool m_bConnect;
	SOCKET m_SocketID;
	// socket address structure
	SOCKADDR_IN m_SockAddr;
	// peer host
	char m_Host[IP_SIZE];
	// peer port
	uint32 m_Port;
	//输入缓冲区
	BufferStream m_InputStream;
	//输出缓冲区
	BufferStream m_OutputStream;
	//消息队列
	que_Pack m_recvQueue;
};

#endif // MySocket_h__