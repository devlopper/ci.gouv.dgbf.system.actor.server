package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.server.Helper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.AdministrativeUnit;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestType;
import ci.gouv.dgbf.system.actor.server.persistence.entities.ScopeFunction;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class RequestScopeFunctionRequestIdentifierRequestCodeIdentityScopeFunctionStringGrantedStringReader extends AbstractRequestScopeFunctionReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getTuple().addJoins("LEFT JOIN AdministrativeUnit au ON au = t.request.administrativeUnit","LEFT JOIN Section s ON s = au.section");
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",RequestScopeFunction.FIELD_IDENTIFIER,"request.identifier","request.code","scopeFunction.identifier",RequestScopeFunction.FIELD_GRANTED)
		.addFromTuple("t.scopeFunction.function", Function.FIELD_CODE)
		.addFromTuple("t.request.type", RequestType.FIELD_CREDIT_MANAGER_SIGNATURE_SPECIMEN_REPORT_IDENTIFIER, RequestType.FIELD_AUTHORIZING_OFFICER_SIGNATURE_SPECIMEN_REPORT_IDENTIFIER)
		
		.addFromTuple("t.scopeFunction", ScopeFunction.FIELD_CODE, ScopeFunction.FIELD_NAME)
		.addFromTuple("s", Section.FIELD_CODE/*, Section.FIELD_NAME*/)
		.addFromTuple("au", AdministrativeUnit.FIELD_CODE, AdministrativeUnit.FIELD_NAME)
		.addFromTuple("t.request", Request.FIELD_ELECTRONIC_MAIL_ADDRESS,Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES, Request.FIELD_REGISTRATION_NUMBER
				,Request.FIELD_ADMINISTRATIVE_FUNCTION,Request.FIELD_MOBILE_PHONE_NUMBER,Request.FIELD_OFFICE_PHONE_NUMBER,Request.FIELD_POSTAL_BOX_ADDRESS
				,Request.FIELD_ACT_OF_APPOINTMENT_REFERENCE)
		;
		return arguments;
	}
	
	@Override
	protected void __set__(RequestScopeFunction requestScopeFunction, Object[] array) {
		Integer index = 1;
		requestScopeFunction.setRequestIdentifier(getAsString(array, index++));
		requestScopeFunction.setRequestCode(getAsString(array, index++));
		requestScopeFunction.setScopeFunctionIdentifier(getAsString(array, index++));
		requestScopeFunction.setGrantedString(Helper.ifTrueYesElseNo((Boolean)array[index++]));
		String functionCode = getAsString(array, index++);
		String ri1 = getAsString(array, index++);
		String ri2 = getAsString(array, index++);
		requestScopeFunction.setSignatureSpecimenReportIdentifier(Function.CODE_CREDIT_MANAGER_HOLDER.equals(functionCode) ? ri1 : (Function.CODE_AUTHORIZING_OFFICER_HOLDER.equals(functionCode) ? ri2 : null));
		
		requestScopeFunction.setScopeFunctionString(getAsString(array, index++)+" "+getAsString(array, index++));
		requestScopeFunction.setSectionString(getAsString(array, index++)/*+" "+getAsString(array, index++)*/);
		requestScopeFunction.setAdministrativeUnitString(getAsString(array, index++)+" "+getAsString(array, index++));
		requestScopeFunction.setElectronicMailAddress(getAsString(array, index++));
		requestScopeFunction.setFirstName(getAsString(array, index++));
		requestScopeFunction.setLastNames(getAsString(array, index++));
		requestScopeFunction.setRegistrationNumber(getAsString(array, index++));
		requestScopeFunction.setAdministrativeUnitFunction(getAsString(array, index++));
		requestScopeFunction.setMobilePhoneNumber(getAsString(array, index++));
		requestScopeFunction.setOfficePhoneNumber(getAsString(array, index++));
		requestScopeFunction.setPostalBoxAddress(getAsString(array, index++));
		requestScopeFunction.setActOfAppointmentReference(getAsString(array, index++));
	}
}