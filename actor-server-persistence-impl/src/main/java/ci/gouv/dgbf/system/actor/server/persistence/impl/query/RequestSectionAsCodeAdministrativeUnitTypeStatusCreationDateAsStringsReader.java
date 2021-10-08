package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

public class RequestSectionAsCodeAdministrativeUnitTypeStatusCreationDateAsStringsReader extends RequestSectionAdministrativeUnitTypeStatusCreationDateAsStringsReader implements Serializable {

	protected String getSectionProjection() {
		return "s.code";
	}
}