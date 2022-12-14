
// Generated by Akka gRPC. DO NOT EDIT.
package org.apache.openwhisk.grpc

/**
 * #services
 */
trait ActivationService {
  
  
  def fetchActivation(in: org.apache.openwhisk.grpc.FetchRequest): scala.concurrent.Future[org.apache.openwhisk.grpc.FetchResponse]
  
  
  def rescheduleActivation(in: org.apache.openwhisk.grpc.RescheduleRequest): scala.concurrent.Future[org.apache.openwhisk.grpc.RescheduleResponse]
  
}

object ActivationService extends akka.grpc.ServiceDescription {
  val name = "activation.ActivationService"

  val descriptor: com.google.protobuf.Descriptors.FileDescriptor =
    org.apache.openwhisk.grpc.ActivationProto.javaDescriptor;

  object Serializers {
    import akka.grpc.scaladsl.ScalapbProtobufSerializer
    
    val FetchRequestSerializer = new ScalapbProtobufSerializer(org.apache.openwhisk.grpc.FetchRequest.messageCompanion)
    
    val RescheduleRequestSerializer = new ScalapbProtobufSerializer(org.apache.openwhisk.grpc.RescheduleRequest.messageCompanion)
    
    val FetchResponseSerializer = new ScalapbProtobufSerializer(org.apache.openwhisk.grpc.FetchResponse.messageCompanion)
    
    val RescheduleResponseSerializer = new ScalapbProtobufSerializer(org.apache.openwhisk.grpc.RescheduleResponse.messageCompanion)
    
  }
}
