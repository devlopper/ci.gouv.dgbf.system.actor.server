package ci.gouv.dgbf.system.actor.server.persistence.impl.query;

import java.io.Serializable;

public class ReqSecAsCodeAdmtvUnitAsCodTypStatusCreationDateAsStringsReader extends RequestSectionAsCodeAdministrativeUnitTypeStatusCreationDateAsStringsReader implements Serializable {

	protected String getAdministrativeUnitProjection() {
		return "au.code";
	}
}