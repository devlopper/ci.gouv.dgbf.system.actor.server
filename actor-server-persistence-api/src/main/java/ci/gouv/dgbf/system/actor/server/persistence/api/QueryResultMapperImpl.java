package ci.gouv.dgbf.system.actor.server.persistence.api;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.ZoneOffset;

import org.cyk.utility.__kernel__.computation.ComparisonOperator;
import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.__kernel__.persistence.query.QueryResultMapper;
import org.cyk.utility.__kernel__.time.TimeHelper;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AccountRequest;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;

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
				Boolean shared = Boolean.TRUE.equals(NumberHelper.compare(scopeFunction.getNumberOfActor(),1,ComparisonOperator.GT));
				scopeFunction.setSharedAsString(shared ? "Oui" : "Non");
				return;
			}
		}
		super.write(instance, fieldName, value);
	}
	
}