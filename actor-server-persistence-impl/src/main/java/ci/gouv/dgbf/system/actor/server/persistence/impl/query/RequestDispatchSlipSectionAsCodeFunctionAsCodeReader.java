package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

public class RequestDispatchSlipSectionAsCodeFunctionAsCodeReader extends RequestDispatchSlipSectionFunctionAsStringReader implements Serializable {

	@Override
	protected String getSectionProjection() {
		return "s.code";
	}
	
	@Override
	protected String getFunctionProjection() {
		return "f.code";
	}
}