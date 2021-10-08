package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

public class RequestSectionAsCodeAdministrativeUnitAsCodeTypeStatusCreationDateAsStringsReader extends RequestSectionAsCodeAdministrativeUnitTypeStatusCreationDateAsStringsReader implements Serializable {

	protected String getAdministrativeUnitProjection() {
		return "au.code";
	}
}