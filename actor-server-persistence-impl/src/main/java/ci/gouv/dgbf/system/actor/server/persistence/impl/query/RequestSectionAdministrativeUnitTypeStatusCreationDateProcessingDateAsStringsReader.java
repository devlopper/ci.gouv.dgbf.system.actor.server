package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;
import java.time.LocalDateTime;

import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public class RequestSectionAdministrativeUnitTypeStatusCreationDateProcessingDateAsStringsReader extends RequestSectionAdministrativeUnitTypeStatusCreationDateAsStringsReader implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Request.FIELD_PROCESSING_DATE);
		return arguments;
	}
	
	@Override
	protected void __set__(Request request, Object[] array, Integer index) {
		super.__set__(request, array, index);
		request.setProcessingDateAsString(formatLocalDateTime((LocalDateTime)array[index++]));
	}
}