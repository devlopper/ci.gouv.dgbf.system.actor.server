package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

import org.cyk.utility.persistence.server.query.string.QueryStringBuilder;

import ci.gouv.dgbf.system.actor.server.persistence.entities.Function;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Request;
import ci.gouv.dgbf.system.actor.server.persistence.entities.RequestDispatchSlip;
import ci.gouv.dgbf.system.actor.server.persistence.entities.Section;

public class RequestDispatchSlipSectionFunctionReader extends AbstractRequestDispatchSlipReaderImpl implements Serializable {

	@Override
	protected QueryStringBuilder.Arguments instantiateQueryStringBuilderArguments() {
		QueryStringBuilder.Arguments arguments =  super.instantiateQueryStringBuilderArguments();
		arguments.getProjection(Boolean.TRUE).addFromTuple("t",Request.FIELD_IDENTIFIER)
			.addFromTuple("s", Section.FIELD_IDENTIFIER, Section.FIELD_CODE, Section.FIELD_NAME)
			.addFromTuple("f", Function.FIELD_IDENTIFIER, Function.FIELD_CODE, Function.FIELD_NAME);
		arguments.getTuple(Boolean.TRUE).addJoins("JOIN Section s ON s = t.section","JOIN Function f ON f = t.function");
		return arguments;
	}
	
	@Override
	protected void __set__(RequestDispatchSlip requestDispatchSlip, Object[] array) {
		Integer index = 1;
		requestDispatchSlip.setSection(new Section().setIdentifier(getAsString(array, index++)).setCode(getAsString(array, index++)).setName(getAsString(array, index++)));
		requestDispatchSlip.setFunction(new Function().setIdentifier(getAsString(array, index++)).setCode(getAsString(array, index++)).setName(getAsString(array, index++)));
	}
}