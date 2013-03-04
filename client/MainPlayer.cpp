#include "StdAfx.h"
#include "MainPlayer.h"
#include "MySocket.h"
#include "client.h"

typedef void (MainPlayer::*PacketHandle)(WorldPacket &pack);
typedef stdext::hash_map<uint16, PacketHandle> hash_Handle;
hash_Handle g_hashPackHandle;

initialiseSingleton(MainPlayer);
MainPlayer::MainPlayer()
{
	m_nStepError = 0;

	//³õÊ¼»¯WinSock
	WSADATA wsaData;
	if (0!=WSAStartup( MAKEWORD(2, 2), &wsaData) ||
		(LOBYTE(wsaData.wVersion) != 2 || HIBYTE(wsaData.wVersion) != 2 ))
	{
		WSACleanup();
	}

	m_pSocket = new MySocket();
	if (m_pSocket->Create())
	{
		m_pSocket->SetLinger(3);
		m_pSocket->SetNonRecvBuf();
		m_pSocket->SetNonSendBuf();
	}
	g_hashPackHandle[10001] = &MainPlayer::HandleLogin;
	g_hashPackHandle[10002] = &MainPlayer::HandlePath;
	g_hashPackHandle[10003] = &MainPlayer::HandleAddObj;
	g_hashPackHandle[10004] = &MainPlayer::HandleRemoveObj;
}

MainPlayer::~MainPlayer()
{
	WSACleanup();
}

void* MainPlayer::operator new(std::size_t sz)
{
	return ::operator new(sz);
}

void MainPlayer::operator delete(void* p, std::size_t)
{
	::operator delete(p);
}

void MainPlayer::OnSocketEvent(SOCKET sock, int err, int evt)
{
	if (err != 0)
	{
		printf("socket err %d\n", err);
		OnDisConnect();
		return;
	}

	if (m_pSocket == NULL || m_pSocket->GetSocket() != sock)
	{
		return;
	}

	switch (evt)
	{
	case FD_CONNECT:
		{
			//printf("on connect\n");
			m_pSocket->OnConnect();
			WorldPacket pack(10001, 0);
			SendPacket(pack);
		}
		break;
	case FD_READ:
		{
			if (m_pSocket->Receive() < 0)
			{
				OnDisConnect();
			}
		}
		break;
	case FD_WRITE:
		{
			if (m_pSocket->Send() != 0)
			{
				OnDisConnect();
			}
		}
		break;
	case FD_CLOSE:
		{
			printf("on close\n");
			OnDisConnect();
		}
		break;
	default: break;
	}
}

void MainPlayer::LoginServer(const char* szServerAddr, uint32 nServerPort)
{
	if (m_pSocket && !IsLogin())
	{
		long flags = FD_CONNECT | FD_READ | FD_WRITE | FD_CLOSE;
		if (!m_pSocket->IsValid())
			m_pSocket->Create();
		SOCKET s = m_pSocket->GetSocket();
		int ret = ::WSAAsyncSelect(s, sClient.GetWnd(), WM_SOCKET, flags);
		if (ret == 0)
		{
			m_pSocket->Connect(szServerAddr, nServerPort);
		}
	}
}

bool MainPlayer::CanSend() const
{
	return m_pSocket && m_pSocket->IsConnect();
}

void MainPlayer::SendPacket(const WorldPacket &packet)
{
	if (CanSend())
	{
		m_pSocket->Send(packet);
	}
}

void MainPlayer::Run(uint32 elapse)
{
	que_Pack& recvPack = m_pSocket->m_recvQueue;
	while (!recvPack.empty())
	{
		WorldPacket& packet = recvPack.front();
		hash_Handle::iterator it = g_hashPackHandle.find(packet.GetOpcode());
		if (it != g_hashPackHandle.end())
		{
			(this->*it->second)(packet);
		}
		recvPack.pop_front();
	}

	static uint32 nElapse = 0;
	nElapse += elapse;
	if (nElapse > 10000)
	{
		nElapse -= 10000;
		sPlayerMain.SendPacket(WorldPacket(1, 0));
	}

	__super::Run(elapse);
}

bool MainPlayer::MoveTo(int x, int y)
{
	if (IsLogin() && GetSelect())
	{
		return Player::MoveTo(x, y);
	}
	return false;
}

void MainPlayer::OnMoveTo(int x, int y)
{
	if (m_nStepError >= 3)
		return;

	WorldPacket pack(10002);
	pack << x << y;
	SendPacket(pack);
	++m_nStepError;
}

void MainPlayer::SetSelect(int x, int y)
{
	if (x == GetSceneX() && y == GetSceneY())
		Player::SetSelect(!GetSelect());
}

void MainPlayer::OnDisConnect()
{
	m_ID = 0;
	if (m_pSocket)
		m_pSocket->Close();
}
