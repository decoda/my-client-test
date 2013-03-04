#include "StdAfx.h"
#include "MySocket.h"
#include "Msg.h"

#define DEFAULT_INPUTSTREAM_SIZE 512	// 输入缓冲大小
#define DEFAULT_OUTPUTSTREAM_SIZE 512  // 输出缓冲大小
#define HEAD_LENGTH 5 // 包头大小

MySocket::MySocket()
: m_InputStream(DEFAULT_INPUTSTREAM_SIZE)
, m_OutputStream(DEFAULT_OUTPUTSTREAM_SIZE)
{
	m_SocketID = INVALID_SOCKET;
	memset(&m_SockAddr, 0, sizeof(SOCKADDR_IN) );
	memset(m_Host, 0, IP_SIZE );
	m_Port = 0;
	m_bConnect = false;
}

MySocket::MySocket(const char* host, uint32 port)
: m_InputStream(DEFAULT_INPUTSTREAM_SIZE)
, m_OutputStream(DEFAULT_OUTPUTSTREAM_SIZE)
{
	strncpy(m_Host, host, IP_SIZE-1);
	m_Port = port;
	m_SocketID = ::socket(AF_INET, SOCK_STREAM, 0);
	memset( &m_SockAddr , 0 , sizeof(m_SockAddr) );
	m_SockAddr.sin_family = AF_INET;
	m_bConnect = false;
}

MySocket::~MySocket()
{
}

void MySocket::Close()
{
	::closesocket(m_SocketID);
	m_SocketID = INVALID_SOCKET;
	memset(&m_SockAddr, 0, sizeof(SOCKADDR_IN) );
	memset(m_Host, 0, IP_SIZE );
	m_Port = 0;
	m_bConnect = false;
}

bool MySocket::Connect()
{
	m_SockAddr.sin_addr.s_addr = inet_addr( m_Host);
	// set sockaddr's port
	m_SockAddr.sin_port = htons(m_Port);
	::connect(m_SocketID, (const sockaddr *)&m_SockAddr, sizeof(m_SockAddr));
	return true;
}

bool MySocket::Connect(const char* host, uint32 port)
{
	strncpy(m_Host, host, IP_SIZE);
	m_Port = port;
	return Connect();
}

uint32 MySocket::Available() const
{
	uint32 argp = 0;
	::ioctlsocket(m_SocketID, FIONREAD, (u_long *)&argp);
	return argp;
}

int32 MySocket::Send(const void* buff, int32 len)
{
	if (m_OutputStream.Write(buff, len))
		return m_OutputStream.SocketFlush(this);
	return 0;
}

int32 MySocket::Send()
{
	return m_OutputStream.SocketFlush(this);
}

int32 MySocket::Send(const WorldPacket &packet)
{
	int32 size = packet.size();
	uint16 len = HEAD_LENGTH + size;
	int8 iszip = 0;
	uint16 cmd = packet.GetOpcode();
	stMsgHead head(len, iszip, cmd);
	m_OutputStream.Write(&head, sizeof(head));
	if (size > 0)
		m_OutputStream.Write(packet.contents(), size);
	return m_OutputStream.SocketFlush(this);
}

int32 MySocket::Receive()
{
	int32 ret = m_InputStream.SocketFill(this);
	if (ret > 0)
	{
		while (true)
		{
			stMsgHead head;
			if (m_InputStream.Read(&head, sizeof(stMsgHead)))
			{
				uint16 len = ntohs(head.len);
				uint16 cmd = ntohs(head.cmd);
				int32 nSize = len - sizeof(stMsgHead);
				if (nSize > 0)
				{
					if ((int32)m_InputStream.Size() < nSize)
						break;
				}
				else if (nSize < 0)
				{
					ASSERT(0);
				}

				WorldPacket pack(cmd, nSize);
				pack.resize(nSize);
				if (m_InputStream.Read((uint8*)pack.contents(), nSize))
				{
					m_recvQueue.push_back(pack);
				}
			}
			else
			{
				break;
			}
		}
	}
	return ret;
}

bool MySocket::SetLinger(uint32 lingertime)
{
	struct linger ling;
	ling.l_onoff = lingertime > 0 ? 1 : 0;
	ling.l_linger = lingertime;
	int ret = ::setsockopt(m_SocketID, SOL_SOCKET, SO_LINGER, (const char*)&ling, sizeof(ling));
	if (ret == SOCKET_ERROR)
	{
		return false;
	}
	return true;
}

bool MySocket::SetNonBlocking(bool on)
{
	u_long argp = on ? 1 : 0;
	int ret = ::ioctlsocket(m_SocketID, FIONBIO, &argp);
	if (ret == SOCKET_ERROR)
	{
		return false;
	}
	return true;
}

bool MySocket::SetNonSendBuf(bool on /*= true*/)
{
	int nZero = 0;
	int ret = ::setsockopt(m_SocketID, SOL_SOCKET, SO_SNDBUF, (char*)&nZero, sizeof(nZero));
	if (ret == SOCKET_ERROR)
	{
		return false;
	}
	return true;
}

bool MySocket::SetNonRecvBuf(bool on /*= true*/)
{
	int nZero = 0;
	int ret = ::setsockopt(m_SocketID, SOL_SOCKET, SO_RCVBUF, (char*)&nZero, sizeof(nZero));
	if (ret == SOCKET_ERROR)
	{
		return false;
	}
	return true;
}

bool MySocket::Create()
{
	m_SocketID = ::socket(AF_INET, SOCK_STREAM, 0);
	memset(&m_SockAddr, 0, sizeof(m_SockAddr));
	m_SockAddr.sin_family = AF_INET;
	m_InputStream.Clear();
	m_OutputStream.Clear();
	m_recvQueue.clear();
	return IsValid();
}
