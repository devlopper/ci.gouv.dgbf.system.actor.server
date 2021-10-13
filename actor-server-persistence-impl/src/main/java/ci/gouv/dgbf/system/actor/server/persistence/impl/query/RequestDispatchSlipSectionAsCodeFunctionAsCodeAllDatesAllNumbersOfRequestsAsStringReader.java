package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.__kernel__.number.NumberHelper;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestStatus;

public class RequestDispatchSlipSectionAsCodeFunctionAsCodeAllDatesAllNumbersOfRequestsAsStringReader extends AbstractRequestDispatchSlipReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",RequestDispatchSlip.FIELD_IDENTIFIER).add("s.code","f.code");
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",RequestDispatchSlip.FIELD_CREATION_DATE,RequestDispatchSlip.FIELD_SENDING_DATE
				,RequestDispatchSlip.FIELD_PROCESSING_DATE);
		arguments.getProjection(Boolean.TRUE).add(
				"(SELECT COUNT(r.identifier) FROM Request r WHERE r.dispatchSlip = t)"
				,String.format("(SELECT COUNT(r.identifier) FROM Request r WHERE r.dispatchSlip = t AND r.status.code = '%s')",RequestStatus.CODE_ACCEPTED)
				,String.format("(SELECT COUNT(r.identifier) FROM Request r WHERE r.dispatchSlip = t AND r.status.code = '%s')",RequestStatus.CODE_REJECTED)
				);
		arguments.getTuple(Boolean.TRUE).addJoins("JOIN Section s ON s = t.section","JOIN Function f ON f = t.function");
		return arguments;
	}
	
	@Override
	protected void __set__(RequestDispatchSlip requestDispatchSlip, Object[] array) {
		Integer index = 1;
		requestDispatchSlip.setSectionAsString(getAsString(array, index++));
		requestDispatchSlip.setFunctionAsString(getAsString(array, index++));
		requestDispatchSlip.setCreationDateAsString(formatLocalDateTime(array, index++));
		requestDispatchSlip.setSendingDateAsString(formatLocalDateTime(array, index++));
		requestDispatchSlip.setProcessingDateAsString(formatLocalDateTime(array, index++));
		requestDispatchSlip.setNumberOfRequests(NumberHelper.getInteger(getAsLong(array, index++)));
		requestDispatchSlip.setNumberOfRequestsAccepted(NumberHelper.getInteger(getAsLong(array, index++)));
		requestDispatchSlip.setNumberOfRequestsRejected(NumberHelper.getInteger(getAsLong(array, index++)));
		requestDispatchSlip.setNumberOfRequestsProcessed(NumberHelper.getInteger(NumberHelper.subtract(requestDispatchSlip.getNumberOfRequestsAccepted(),requestDispatchSlip.getNumberOfRequestsRejected())));
		requestDispatchSlip.setNumberOfRequestsNotProcessed(NumberHelper.getInteger(NumberHelper.subtract(requestDispatchSlip.getNumberOfRequests(),requestDispatchSlip.getNumberOfRequestsProcessed())));
	}
}