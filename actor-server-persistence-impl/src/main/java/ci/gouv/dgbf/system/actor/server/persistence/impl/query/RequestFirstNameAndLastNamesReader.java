package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;

public class RequestFirstNameAndLastNamesReader extends AbstractRequestReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Request.FIELD_IDENTIFIER).add(Language.Select.concat("t", Request.FIELD_FIRST_NAME,Request.FIELD_LAST_NAMES));
		arguments.getTuple(Boolean.TRUE).addJoins("LEFT JOIN RequestDispatchSlip rds ON rds = t.dispatchSlip");
		return arguments;
	}
	
	@Override
	protected void __set__(Request request, Object[] array) {
		Integer index = 1;
		request.setFirstNameAndLastNames(getAsString(array, index));
	}
}