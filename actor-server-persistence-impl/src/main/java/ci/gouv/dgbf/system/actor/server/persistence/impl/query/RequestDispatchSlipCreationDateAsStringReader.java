package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;

public class RequestDispatchSlipCreationDateAsStringReader extends AbstractRequestDispatchSlipReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",RequestDispatchSlip.FIELD_IDENTIFIER,RequestDispatchSlip.FIELD_CREATION_DATE);
		return arguments;
	}
	
	@Override
	protected void __set__(RequestDispatchSlip requestDispatchSlip, Object[] array) {
		Integer index = 1;
		requestDispatchSlip.setCreationDateAsString(formatLocalDateTime(array,index));
	}
}