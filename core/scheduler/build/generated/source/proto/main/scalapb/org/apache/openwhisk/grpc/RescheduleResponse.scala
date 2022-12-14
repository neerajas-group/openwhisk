// Generated by the Scala Plugin for the Protocol Buffer Compiler.
// Do not edit!
//
// Protofile syntax: PROTO3

package org.apache.openwhisk.grpc

/** @param isRescheduled
  *   if reschedule request is failed, then it will be `false`
  */
@SerialVersionUID(0L)
final case class RescheduleResponse(
    isRescheduled: _root_.scala.Boolean = false,
    unknownFields: _root_.scalapb.UnknownFieldSet = _root_.scalapb.UnknownFieldSet.empty
    ) extends scalapb.GeneratedMessage with scalapb.lenses.Updatable[RescheduleResponse] {
    @transient
    private[this] var __serializedSizeCachedValue: _root_.scala.Int = 0
    private[this] def __computeSerializedValue(): _root_.scala.Int = {
      var __size = 0
      
      {
        val __value = isRescheduled
        if (__value != false) {
          __size += _root_.com.google.protobuf.CodedOutputStream.computeBoolSize(1, __value)
        }
      };
      __size += unknownFields.serializedSize
      __size
    }
    override def serializedSize: _root_.scala.Int = {
      var read = __serializedSizeCachedValue
      if (read == 0) {
        read = __computeSerializedValue()
        __serializedSizeCachedValue = read
      }
      read
    }
    def writeTo(`_output__`: _root_.com.google.protobuf.CodedOutputStream): _root_.scala.Unit = {
      {
        val __v = isRescheduled
        if (__v != false) {
          _output__.writeBool(1, __v)
        }
      };
      unknownFields.writeTo(_output__)
    }
    def withIsRescheduled(__v: _root_.scala.Boolean): RescheduleResponse = copy(isRescheduled = __v)
    def withUnknownFields(__v: _root_.scalapb.UnknownFieldSet) = copy(unknownFields = __v)
    def discardUnknownFields = copy(unknownFields = _root_.scalapb.UnknownFieldSet.empty)
    def getFieldByNumber(__fieldNumber: _root_.scala.Int): _root_.scala.Any = {
      (__fieldNumber: @_root_.scala.unchecked) match {
        case 1 => {
          val __t = isRescheduled
          if (__t != false) __t else null
        }
      }
    }
    def getField(__field: _root_.scalapb.descriptors.FieldDescriptor): _root_.scalapb.descriptors.PValue = {
      _root_.scala.Predef.require(__field.containingMessage eq companion.scalaDescriptor)
      (__field.number: @_root_.scala.unchecked) match {
        case 1 => _root_.scalapb.descriptors.PBoolean(isRescheduled)
      }
    }
    def toProtoString: _root_.scala.Predef.String = _root_.scalapb.TextFormat.printToUnicodeString(this)
    def companion = org.apache.openwhisk.grpc.RescheduleResponse
}

object RescheduleResponse extends scalapb.GeneratedMessageCompanion[org.apache.openwhisk.grpc.RescheduleResponse] {
  implicit def messageCompanion: scalapb.GeneratedMessageCompanion[org.apache.openwhisk.grpc.RescheduleResponse] = this
  def merge(`_message__`: org.apache.openwhisk.grpc.RescheduleResponse, `_input__`: _root_.com.google.protobuf.CodedInputStream): org.apache.openwhisk.grpc.RescheduleResponse = {
    var __isRescheduled = `_message__`.isRescheduled
    var `_unknownFields__`: _root_.scalapb.UnknownFieldSet.Builder = null
    var _done__ = false
    while (!_done__) {
      val _tag__ = _input__.readTag()
      _tag__ match {
        case 0 => _done__ = true
        case 8 =>
          __isRescheduled = _input__.readBool()
        case tag =>
          if (_unknownFields__ == null) {
            _unknownFields__ = new _root_.scalapb.UnknownFieldSet.Builder(_message__.unknownFields)
          }
          _unknownFields__.parseField(tag, _input__)
      }
    }
    org.apache.openwhisk.grpc.RescheduleResponse(
        isRescheduled = __isRescheduled,
        unknownFields = if (_unknownFields__ == null) _message__.unknownFields else _unknownFields__.result()
    )
  }
  implicit def messageReads: _root_.scalapb.descriptors.Reads[org.apache.openwhisk.grpc.RescheduleResponse] = _root_.scalapb.descriptors.Reads{
    case _root_.scalapb.descriptors.PMessage(__fieldsMap) =>
      _root_.scala.Predef.require(__fieldsMap.keys.forall(_.containingMessage == scalaDescriptor), "FieldDescriptor does not match message type.")
      org.apache.openwhisk.grpc.RescheduleResponse(
        isRescheduled = __fieldsMap.get(scalaDescriptor.findFieldByNumber(1).get).map(_.as[_root_.scala.Boolean]).getOrElse(false)
      )
    case _ => throw new RuntimeException("Expected PMessage")
  }
  def javaDescriptor: _root_.com.google.protobuf.Descriptors.Descriptor = ActivationProto.javaDescriptor.getMessageTypes.get(3)
  def scalaDescriptor: _root_.scalapb.descriptors.Descriptor = ActivationProto.scalaDescriptor.messages(3)
  def messageCompanionForFieldNumber(__number: _root_.scala.Int): _root_.scalapb.GeneratedMessageCompanion[_] = throw new MatchError(__number)
  lazy val nestedMessagesCompanions: Seq[_root_.scalapb.GeneratedMessageCompanion[_ <: _root_.scalapb.GeneratedMessage]] = Seq.empty
  def enumCompanionForFieldNumber(__fieldNumber: _root_.scala.Int): _root_.scalapb.GeneratedEnumCompanion[_] = throw new MatchError(__fieldNumber)
  lazy val defaultInstance = org.apache.openwhisk.grpc.RescheduleResponse(
    isRescheduled = false
  )
  implicit class RescheduleResponseLens[UpperPB](_l: _root_.scalapb.lenses.Lens[UpperPB, org.apache.openwhisk.grpc.RescheduleResponse]) extends _root_.scalapb.lenses.ObjectLens[UpperPB, org.apache.openwhisk.grpc.RescheduleResponse](_l) {
    def isRescheduled: _root_.scalapb.lenses.Lens[UpperPB, _root_.scala.Boolean] = field(_.isRescheduled)((c_, f_) => c_.copy(isRescheduled = f_))
  }
  final val ISRESCHEDULED_FIELD_NUMBER = 1
  def of(
    isRescheduled: _root_.scala.Boolean
  ): _root_.org.apache.openwhisk.grpc.RescheduleResponse = _root_.org.apache.openwhisk.grpc.RescheduleResponse(
    isRescheduled
  )
}
