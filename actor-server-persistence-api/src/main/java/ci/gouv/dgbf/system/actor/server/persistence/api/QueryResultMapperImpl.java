package ci.gouv.dgbf.system.actor.server.persistence.api;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryResultMapper;
import org.cyk.utility.__kernel__.time.TimeHelper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.IdentificationFormAttribute;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeTypeFunction;

@ci.gouv.dgbf.system.actor.server.annotation.System
public class QueryResultMapperImpl extends QueryResultMapper.AbstractImpl implements Serializable {

	@Override
	protected <T> void write(T instance, String fieldName, Object value) {
		if(instance instanceof AccountRequest) {
			AccountRequest accountRequest = (AccountRequest) instance;
			if(AccountRequest.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_STRING.equals(fieldName) && value != null) {
				accountRequest.setActOfAppointmentSignatureDateAsString(TimeHelper.formatLocalDate((LocalDate) value,"dd/MM/yyyy"));
				return;
			}
			if(AccountRequest.FIELD_ACT_OF_APPOINTMENT_SIGNATURE_DATE_AS_TIMESTAMP.equals(fieldName) && value != null) {
				accountRequest.setActOfAppointmentSignatureDateAsTimestamp(((LocalDate) value).atTime(0,0).toInstant(ZoneOffset.UTC).toEpochMilli());
				return;
			}
			if(AccountRequest.FIELD_CREATION_DATE_AS_STRING.equals(fieldName) && value != null) {
				accountRequest.setCreationDateAsString(TimeHelper.formatLocalDateTime((LocalDateTime) value,"dd/MM/yyyy à HH:mm"));
				return;
			}
			if(AccountRequest.FIELD_SUBMISSION_DATE_AS_STRING.equals(fieldName) && value != null) {
				accountRequest.setSubmissionDateAsString(TimeHelper.formatLocalDateTime((LocalDateTime) value,"dd/MM/yyyy à HH:mm"));
				return;
			}
		}else if(instance instanceof ScopeFunction) {
			if(ScopeFunction.FIELD_SHARED_AS_STRING.equals(fieldName)) {
				ScopeFunction scopeFunction = (ScopeFunction) instance;
				scopeFunction.setSharedAsString(ScopeFunctionPersistence.computeSharedAsString( ScopeFunctionPersistence.computeShared(NumberHelper.getInteger(value)) ));
				return;
			}
		}else if(instance instanceof ScopeTypeFunction) {
			if(ScopeTypeFunction.FIELD_SCOPE_FUNCTION_DERIVABLE_AS_STRING.equals(fieldName)) {
				ScopeTypeFunction scopeTypeFunction = (ScopeTypeFunction) instance;
				scopeTypeFunction.setScopeFunctionDerivableAsString(ScopeTypeFunctionPersistence.computeScopeFunctionDerivableAsString((Boolean)value));
				return;
			}
		}else if(instance instanceof Request) {
			Request request = (Request) instance;
			if(Request.FIELD_CREATION_DATE_AS_STRING.equals(fieldName) && value != null) {
				request.setCreationDateAsString(TimeHelper.formatLocalDateTime((LocalDateTime) value,"dd/MM/yyyy à HH:mm"));
				return;
			}
			if(Request.FIELD_PROCESSING_DATE_AS_STRING.equals(fieldName) && value != null) {
				request.setProcessingDateAsString(TimeHelper.formatLocalDateTime((LocalDateTime) value,"dd/MM/yyyy à HH:mm"));
				return;
			}
		}else if(instance instanceof IdentificationFormAttribute) {
			/*IdentificationFormAttribute identificationFormAttribute = (IdentificationFormAttribute) instance;
			if(IdentificationFormAttribute.FIELD_REQUIRED_AS_STRING.equals(fieldName)) {
				identificationFormAttribute.setRequiredAsString(Boolean.TRUE.equals(value) ? "Oui" : "Non");
				return;
			}*/
		}
		super.write(instance, fieldName, value);
	}
	
}