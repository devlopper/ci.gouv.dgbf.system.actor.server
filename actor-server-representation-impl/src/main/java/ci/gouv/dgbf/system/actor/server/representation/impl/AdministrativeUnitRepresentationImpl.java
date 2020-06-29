package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.AdministrativeUnitRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.AdministrativeUnitDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class AdministrativeUnitRepresentationImpl extends AbstractRepresentationEntityImpl<AdministrativeUnitDto> implements AdministrativeUnitRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
