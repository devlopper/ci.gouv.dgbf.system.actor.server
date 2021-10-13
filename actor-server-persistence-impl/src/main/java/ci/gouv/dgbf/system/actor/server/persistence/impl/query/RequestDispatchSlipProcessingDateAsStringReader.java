package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;

public class RequestDispatchSlipProcessingDateAsStringReader extends AbstractRequestDispatchSlipReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_PROCESSING_DATE);
		return arguments;
	}
	
	@Override
	protected void __set__(RequestDispatchSlip requestDispatchSlip, Object[] array) {
		Integer index = 1;
		requestDispatchSlip.setProcessingDateAsString(formatLocalDateTime(array,index));
	}
}