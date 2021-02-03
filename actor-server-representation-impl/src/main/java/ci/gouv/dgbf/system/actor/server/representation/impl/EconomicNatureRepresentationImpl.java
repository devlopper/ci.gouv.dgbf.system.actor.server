package ci.gouv.dgbf.system.actor.server.representation.impl;

import java.io.Serializable;

import javax.enterprise.context.ApplicationScoped;

import ci.gouv.dgbf.system.actor.server.representation.api.EconomicNatureRepresentation;
import ci.gouv.dgbf.system.actor.server.representation.entities.EconomicNatureDto;
import org.cyk.utility.server.representation.AbstractRepresentationEntityImpl;

@ApplicationScoped
public class EconomicNatureRepresentationImpl extends AbstractRepresentationEntityImpl<EconomicNatureDto> implements EconomicNatureRepresentation,Serializable {
	private static final long serialVersionUID = 1L;
	
}
