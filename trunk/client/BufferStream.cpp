#include "StdAfx.h"
#include "BufferStream.h"
#include "MySocket.h"

BufferStream::BufferStream(uint32 bufferlen)
{
	m_nHead = m_nTail = 0;
	m_nBufferLen = bufferlen;
	m_buffer = new char[m_nBufferLen];
	::memset(m_buffer, 0, m_nBufferLen);
}


BufferStream::~BufferStream()
{
	if (m_buffer != NULL)
		delete []m_buffer;
}

uint32 BufferStream::Size() const
{
	if (m_nHead < m_nTail)
		return m_nTail - m_nHead;
	else if(m_nHead > m_nTail)
		return m_nBufferLen - m_nHead + m_nTail;
	return 0;
}

uint32 BufferStream::Space() const
{
	if (m_nHead < m_nTail)
		return m_nBufferLen - m_nTail + m_nHead;
	else if(m_nHead > m_nTail)
		return m_nHead - m_nTail;
	else
		return m_nBufferLen;
}

bool BufferStream::Read(void* destination, size_t bytes)
{
	if (bytes == 0)
		return false;

	uint32 nsize = Size();
	if (bytes > nsize)
		return false;

	if (m_nHead <= m_nTail)
	{
		memcpy(destination, &m_buffer[m_nHead], bytes);
	}
	else
	{
		uint32 nleft = m_nBufferLen - m_nHead;
		if (nleft >= bytes)
		{
			memcpy(destination, &m_buffer[m_nHead], bytes);
		}
		else
		{
			char *p = (char*)destination;
			memcpy(p, &m_buffer[m_nHead], nleft);
			p += nleft;
			memcpy(p, m_buffer, bytes - nleft);
		}
	}

	m_nHead = (m_nHead + bytes) % m_nBufferLen;
	return true;
}

bool BufferStream::Write(const void* data, size_t bytes)
{
	if (bytes == 0)
		return false;

	uint32 nfree = Space();
	if (bytes >= nfree)
	{
		if (!Resize())
			return false;
	}

	if (m_nHead <= m_nTail)
	{
		if (m_nHead == 0)
		{
			memcpy(&m_buffer[m_nTail], (char*)data, bytes);
		}
		else
		{
			nfree = m_nBufferLen - m_nTail;
			if (bytes > nfree)
			{
				memcpy(&m_buffer[m_nTail], (char*)data, nfree);
				memcpy(m_buffer, (char*)data + nfree, bytes - nfree);
			}
			else
			{
				memcpy(&m_buffer[m_nTail], (char*)data, bytes);
			}
		}
	}
	else
	{
		memcpy(&m_buffer[m_nTail], (char*)data, bytes);
	}

	m_nTail = (m_nTail + bytes) % m_nBufferLen;
	return true;
}

bool BufferStream::Resize()
{
	if (m_nBufferLen == 0 || m_buffer == NULL)
		return false;

	m_nBufferLen *= 2;
	char *buffer = new char[m_nBufferLen];
	memset(buffer, 0, m_nBufferLen);

	uint32 nsize = Size();
	Read(buffer, Size());

	delete []m_buffer;
	m_buffer = buffer;
	m_nHead = 0;
	m_nTail = nsize;
	return true;
}

// socket¶Á³ö
int32 BufferStream::SocketFlush(MySocket *m_pSocket)
{
	SOCKET sock = m_pSocket->GetSocket();
	int32 nFlushed = 0;
	int32 nLeft, nSend;
	if (m_nHead <= m_nTail)
	{
		nLeft = m_nTail - m_nHead;

		while (nLeft > 0) 
		{
			nSend = Send(sock, nLeft);
			if (nSend == 0) return 0;

			nFlushed += nSend;
			m_nHead += nSend;
			nLeft -= nSend;
		}
	}
	else
	{
		nLeft = m_nBufferLen - m_nHead;
		while (nLeft > 0) 
		{
			nSend = Send(sock, nLeft);
			if (nSend == 0) return 0;

			nFlushed += nSend;
			m_nHead += nSend;
			nLeft -= nSend;
		}

		m_nHead = 0;
		nLeft = m_nTail;

		while (nLeft > 0) 
		{
			nSend = Send(sock, nLeft);
			if (nSend == 0) return 0;

			nFlushed += nSend;
			m_nHead += nSend;
			nLeft -= nSend;
		}
	}

	return nFlushed;
}

// socket¶ÁÈë
int32 BufferStream::SocketFill(MySocket *m_pSocket)
{
	int32 nReceived = 0;
	int32 nFilled = 0;
	int32 nFree = Space();
	if (nFree == 0)
	{
		if (!Resize())
			return nFilled;
		 nFree = Space();
	}

	SOCKET sock = m_pSocket->GetSocket();
	if (m_nHead <= m_nTail)
	{
		if (m_nHead == 0)
		{
			nReceived = Recv(sock, nFree);
			if (nReceived == 0) return 0;

			m_nTail += nReceived;
			nFilled += nReceived;

			if (nReceived == nFree)
			{
				uint32 nAvailable = m_pSocket->Available();
				if (nAvailable > 0)
				{
					if (!Resize())
						return nFilled;

					nReceived = Recv(sock, nFree);
					if (nReceived == 0) return 0;

					m_nTail += nReceived;
					nFilled += nReceived;
				}
			}

		}
		else
		{
			nFree = m_nBufferLen - m_nTail;
			nReceived = Recv(sock, nFree);
			if (nReceived == 0) return 0;

			m_nTail = (m_nTail + nReceived) % m_nBufferLen;
			nFilled += nReceived;

			if (nReceived == nFree)
			{
				nReceived = 0;
				nFree = m_nHead - 1;
				if (nFree != 0)
				{
					nReceived = Recv(sock, nFree);
					if (nReceived == 0) return 0;

					m_nTail += nReceived;
					nFilled += nReceived;
				}

				if (nReceived == nFree)
				{
					uint32 nAvailable = m_pSocket->Available();
					if (nAvailable > 0)
					{
						if (!Resize())
							return nFilled;

						nReceived = Recv(sock, nFree);
						if (nReceived == 0) return 0;

						m_nTail += nReceived;
						nFilled += nReceived;
					}
				}
			}
		}
	}
	else
	{
		nReceived = Recv(sock, nFree);
		if (nReceived == 0) return 0;

		m_nTail += nReceived;
		nFilled += nReceived;

		if (nReceived == nFree)
		{
			uint32 nAvailable = m_pSocket->Available();
			if (nAvailable > 0)
			{
				if (!Resize())
					return nFilled;

				nReceived = Recv(sock, nFree);
				if (nReceived == 0) return 0;

				m_nTail += nReceived;
				nFilled += nReceived;
			}
		}
	}

	return nFilled;
}

int32 BufferStream::Recv(SOCKET s, int32 len)
{
	int32 nReceived = ::recv(s, &m_buffer[m_nTail], len, 0);
	if (nReceived == SOCKET_ERROR)
	{
		if (::WSAGetLastError() == WSAEWOULDBLOCK)
			return 0;
		else
			return SOCKET_ERROR - 2;
	}
	return nReceived;
}

int32 BufferStream::Send(SOCKET s, int32 len)
{
	int32 nSend = ::send(s, &m_buffer[m_nHead] , len , MSG_DONTROUTE);
	if (nSend == SOCKET_ERROR)
	{
		if (::WSAGetLastError() == WSAEWOULDBLOCK)
			return 0;
		else
			return SOCKET_ERROR - 2;
	}
	return nSend;
}
