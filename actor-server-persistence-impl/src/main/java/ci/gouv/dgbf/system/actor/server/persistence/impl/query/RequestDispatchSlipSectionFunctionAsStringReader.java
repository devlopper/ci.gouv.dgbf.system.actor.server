package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.query.Language;
import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;

public class RequestDispatchSlipSectionFunctionAsStringReader extends AbstractRequestDispatchSlipReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",RequestDispatchSlip.FIELD_IDENTIFIER).add(getSectionProjection(),getFunctionProjection());
		arguments.getTuple(Boolean.TRUE).addJoins("JOIN Section s ON s = t.section","JOIN Function f ON f = t.function");
		return arguments;
	}
	
	protected String getSectionProjection() {
		return Language.Select.concatCodeName("s");
	}
	
	protected String getFunctionProjection() {
		return "f.name";
	}
	
	@Override
	protected void __set__(RequestDispatchSlip requestDispatchSlip, Object[] array) {
		Integer index = 1;
		requestDispatchSlip.setSectionAsString(getAsString(array, index++));
		requestDispatchSlip.setFunctionAsString(getAsString(array, index++));
	}
}