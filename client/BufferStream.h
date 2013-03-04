#ifndef BufferStream_h__
#define BufferStream_h__

class MySocket;

class BufferStream
{
public:
	BufferStream(uint32 bufflen);
	~BufferStream(void);

	bool Read(void* destination, size_t bytes);
	bool Write(const void* data, size_t bytes);

	int32 SocketFlush(MySocket *pSocket);
	int32 SocketFill(MySocket *pSocket);

	uint32 Size() const;
	uint32 Space() const;
	bool Resize();
	void Clear()
	{
		m_nHead = m_nTail = 0;
	}

private:
	int32 Recv(SOCKET s, int32 len);
	int32 Send(SOCKET s, int32 len);

private:
	char		*m_buffer;
	uint32		m_nBufferLen;
	uint32		m_nHead;
	uint32		m_nTail;
};
#endif // BufferStream_h__

